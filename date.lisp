(in-package :date)

(defun date! (val)
  (or (date? val) (now)))

(defun date? (val)
  (make-date val))

(defconstant +seconds-per-day+ (* 60 60 24)
  "Number of seconds in a day excluding leap seconds and other oddities")

(defconstant +date-standard-fields+ '(:year :month :day :hour :minute :second :offset :full-offset :short-weekday :short-month :weekday)
  "List of all standard fields available to print")

(defconstant +date-long-fields+ '(:year :month :day :weekday :hour :minute :second)
  "List of all fields including day of week (e.g. Monday) to print")

(defconstant +date-day-fields+ '(:year :month :day)
  "List of only date-specific fields available to print or compare")

(defconstant +iso8601+ '((:year 4) #\- (:month 2) #\- (:day 2) #\T (:hour 2) #\: (:minute 2) #\: (:second 2) :offset)
  "Long format combined date and date in UTC")

(defconstant +natural+ '((:short-weekday 3) #\Space (:day 2) #\- (:month 2) #\- (:year 4) #\Space (:hour 2) #\: (:minute 2) #\: (:second 2))
  "Long format date including weekday in usual English (non-USA) format")

(defconstant +yyyy-mm-dd+ '((:year 4) #\- (:month 2) #\- (:day 2))
  "Short format date excluding date")

(defconstant +natural-short+ '((:day 2) #\- (:month 2) #\- (:year 4) #\Space (:hour 2) #\: (:minute 2) #\: (second 2))
  "Long format date excluding weekday in usual English (non-USA) format")

(defconstant +natural-short-day+ '((:short-weekday 3) #\Space (:day 1) #\Space (:short-month 3) #\Space (:year 4))
  "Short fromat in human readable date")

(defconstant +rfc2822+ '((:short-weekday 3) #\, #\Space (:day 1) #\Space (:short-month 3) #\Space (:year 4) #\Space (:hour 2) #\:
                         (:minute 2) #\: (:second 2) #\Space (:full-offset 5))
  "Long format date including weekday and short month in English format for e-mail RFC[2]822")

(defconstant +rfc3501-search+ '((:day 1) #\- (:month 1) #\- (:year 4))
  "Short format date used for searching in IMAP requests in RFC3501")

(defconstant +http/1.1+ '((:short-weekday 3) #\, #\Space (:day 1) #\Space (:short-month 3) #\Space (:year 4) #\Space (:hour 2) #\:
                          (:minute 2) #\: (:second 2) #\Space "GMT")
  "Long format date including weekday and short month in English format for a HTTP response")

(defconstant +pdf+ '(#\( #\D #\: (:year 4) (:month 2) (:day 2) (:hour 2) (:minute 2) (:second 2) #\- #\0 #\0 #\' #\0 #\0 #\' #\))
  "Long format date used in PDF documents")

(defclass date ()
  ((seconds :initarg :seconds :reader seconds))
  (:documentation "Number of seconds since the epcoch 1 January 1900"))

(defmethod print-object ((date date) stream)
  (princ (format-date date +iso8601+) stream))

(defun datep (date)
  "Is DATE an instance of class DATE"
  (eq (class-name (class-of date)) 'date))

(defmethod clone ((date date))
  (make-instance 'date :seconds (seconds date)))

(defun make-date (arg)
  "Create a new DATE object parsed from ARG"
  (cond
    ((datep arg) (clone arg))
    ((stringp arg) (parse-date arg))
    ((integerp arg) (make-instance 'date :seconds arg))
    (t (make-instance 'date :seconds 0))))

(defun now ()
  "Create a new DATE object"
  (make-instance 'date :seconds (get-universal-time)))

(defmacro with-date ((date year &optional month day hour minute second) &body body)
  (let ((year% (gensym "YEAR")) (month% (gensym "MONTH")) (day% (gensym "DAY"))
        (hour% (gensym "HOUR")) (minute% (gensym "MINUTE")) (second% (gensym "SECOND")))
    `(multiple-value-bind (,second% ,minute% ,hour% ,day% ,month% ,year%) (decode-universal-time (seconds ,date) 0)
       (declare (ignorable ,second% ,minute% ,hour% ,day% ,month%))
       (let ((,year ,year%) ,@(when month `((,month ,month%))) ,@(when day `((,day ,day%))) ,@(when hour `((,hour ,hour%)))
             ,@(when minute `((,minute ,minute%))) ,@(when second `((,second ,second%))))
         ,@body))))

(defun parse-date (str)
  "Parse an RF2C822 date or an ISO8601 date"
  (or (parse-rfc2822-date str) (parse-ymd-date str) (parse-dmy-date str) (now)))

(defun maybe-parse-integer/0 (str)
  (if (stringp str)
      (parse-integer str)
      str))

(defun parse-rfc2822-date (str)
  "Parse a long form RFC2822 date into a DATE"
  (destructuring-bind (&optional date month year hour minute second)
      ;;       w e d,       1 5       j u n       2 0 2 0      1 0 :  1 0 :  1 0     gmt
      (#~m/^(?:\w\w\w,)?\s*(\d\d?)\s*(\w\w\w)?\s*(\d\d\d\d)\s*(\d\d):(\d\d):(\d\d).*$/ str)
    (when (and date month year hour minute second)
      (make-instance 'date :seconds (apply #'encode-universal-time (mapcar #'maybe-parse-integer/0 (list second minute hour date month year 0)))))))

(defun parse-ymd-date (str)
  (destructuring-bind (&optional year month day hour minute second) (#~m/^(\d{4})\D(\d\d?)\D(\d\d?)(?:\D(\d\d?)\D(\d\d?)\D(\d\d?).*)?$/ str)
    (when (and year month day)
      (make-instance 'date :seconds (apply #'encode-universal-time (mapcar #'maybe-parse-integer/0 (list second minute hour day month year 0)))))))

(defun parse-dmy-date (str)
  (destructuring-bind (&optional day month year) (#~m/^(\d\d?)\D(\w{1,3})\D(\d{4})/ str)
    (when (and day month year)
      (make-instance 'date :seconds (apply #'encode-universal-time (mapcar #'maybe-parse-integer/0 (list 0 0 0 day (month-by-short-name month) year 0)))))))

(defun %date-retrieve-value (element date)
  "Retrieve DATE sub-value for keyword ELEMENT"
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (seconds date) 0)
    (case element
      (:year year) (:month month) (:day day) (:hour hour) (:minute minute) (:second second) (t 0))))

(defun date-adjust (date &rest fields)
  "Alter the current DATE by FIELDS - list of designators and numbers.
e.g. (date-adjust *DATE* :day 7 :hour -5). Returns DATE"
  (flet ((%adjust-years (years)
           "Make sure we get February's date right"
           (multiple-value-bind (sec min hour day month year) (decode-universal-time (seconds date) 0)
             (setf (slot-value date 'seconds)
                   (encode-universal-time sec min hour (days-in-february? day month (+ year years)) month (+ year years) 0))))

         (%adjust-months (months)
           "Months are tricky. e.g. 2018-03-31, 2018-03-30, 2018-03-29, and 2018-03-28 less 1 month all equal 2018-02-28"
           (multiple-value-bind (sec min hour day month year) (decode-universal-time (seconds date) 0)
             (multiple-value-bind (yd m) (floor (+ month months) 12)
               (setf day (min day (days-in-month* m (+ year yd)) (days-in-month* month year))
                     (slot-value date 'seconds) (encode-universal-time sec min hour day m (+ year yd) 0))))))

    (let ((years 0) (months 0) (seconds (seconds date)))
      (loop for (element value) on fields by #'cddr do
        (case element
          (:second (incf seconds value))
          (:minute (incf seconds (* 60 value)))
          (:hour (incf seconds (* 60 60 value)))
          (:day (incf seconds (* +seconds-per-day+ value)))
          (:week (incf seconds (* 7 +seconds-per-day+ value)))
          (:month (multiple-value-bind (y m) (floor value 12)
                    (setf months (+ months m)
                          years (+ years y))))
          (:year (incf years value))))
      (setf (slot-value date 'seconds) seconds)
      (unless (zerop months) (%adjust-months months))
      (unless (zerop years) (%adjust-years years))))

  date)

(defun date+ (date &rest fields)
  "Create a new DATE with adjustments per DATE-ADJUST"
  (apply #'date-adjust (make-date date) fields))

(defun date-min (&rest dates)
  (reduce (lambda (a b) (if (< (seconds a) (seconds b)) a b)) dates))

(defun date-max (&rest dates)
  (reduce (lambda (a b) (if (> (seconds a) (seconds b)) b a)) dates))

(defun output-date-part (stream element date year month day hour minute second offset padding)
  "Retrieve DATE sub-value for keyword ELEMENT"
  (case element
    (:offset (if (zerop offset)
                 (write-char #\Z stream)
                 (multiple-value-bind (h s) (floor offset 3600)
                   (format stream "~C~2,'0D:~2,'0D" (if (minusp h) #\- #\+) (abs h) s))))
    (:full-offset (if (zerop offset)
                      (write-string "+0000" stream)
                      (multiple-value-bind (h s) (floor offset 3600)
                        (format stream "~C~2,'0D~2,'0D" (if (minusp h) #\- #\+) (abs h) s))))
    (:weekday (write-string (weekday-name (weekday date)) stream))
    (:short-weekday (write-string (weekday-short-name (weekday date)) stream))
    (:month-name (write-string (month-name month) stream))
    (:short-month (write-string (month-short-name month) stream))
    (t (format stream "~v,'0D" padding (case element (:year year) (:month month) (:day day) (:hour hour) (:minute minute) (:second second) (t 0))))))

(defun format-date (date &optional (format +iso8601+))
  "Return a string formatting DATE in FORMAT (e.g. :iso8601 or :utc) showing FIELDS
[ default set of (:year :month :day :hour :minute :second) ]. "
  (with-output-to-string (stream)
    (let ((total-seconds (seconds date)))
      (multiple-value-bind (s m h d mo y) (decode-universal-time total-seconds 0)
        (loop for e in format
              if (consp e) do (output-date-part stream (car e) date y mo d h m s 0 (cadr e))
                else if (keywordp e) do (output-date-part stream e date y mo d h m s 0 0)
                       else if (characterp e) do (write-char e stream)
                              else if (stringp e) do (write-string e stream))))))

(defun date-string-contains-p (string test pos len)
  (string= string test :start1 pos :end1 (min len (+ pos (length test)))))

(defun format-date-string (date control)
  "Make a string based on CONTROL from the given DATE
'yyyy-mm-dd hh:nn:ss (HH:nn:ss)' => Assuming date is 1/12/2019 at 7.00pm; => '2019-12-01 19:00:00 (07:00:00PM)'
flags are:
d    =>   1 - 31 day of month (no leading 0)
dd   =>  01 - 31 day of month (with leading 0)
ddd  =>  Abbreviated day name (i.e. Mon, Tue, Wed)
dddd =>  Full day name (i.e. Monday, Tuesday, Wednesday)
w    =>   1 - 7  day of week (Sunday = 1)
ww   =>   1 - 52 week of year with no leading 0 (week 1 starts on January 1)
m    =>   1 - 12 month of year (no leading 0)
mm   =>  01 - 12 month of year (with leading 0)
mmm  =>  Abbreviated month name (i.e. Jan, Feb, Mar)
mmmm =>  Full month name (i.e. January, February, March)
y    =>  1 - 366 day of year (no leading 0)
yy   =>  Last two digits of the year
yyyy =>  4 digit year
h    =>  0 - 23 hour of day with no leading 0
hh   =>  0 - 23 hour of day with leading 0
H    =>  1 - 12 hour of day with AM/PM appended (no leading 0)
HH   =>  1 - 12 hour of day with AM/PM appended (with leading 0)
n    =>  0 - 59 minute of day with no leading 0
nn   =>  0 - 59 minute of day with leading 0
s    =>  0 - 59 second of day with no leading 0
ss   =>  0 - 59 second of day with leading 0
characters not in this list are passed through to the string"
  (multiple-value-bind (seconds minutes hours day month year) (decode-universal-time (seconds date))
    (with-output-to-string (output)
      (loop with end = (length control)
            with am/pm = nil
            for i below end
            for c = (char control i)
            for i4 = (min (+ i 4) end)
            for i3 = (min (+ i 3) end)
            for i2 = (min (+ i 2) end)
            if (string= control "dddd" :start1 i :end1 i4)
              do (incf i 3) (write-string (weekday-name (weekday date)) output)
            else if (string= control "ddd" :start1 i :end1 i3)
                   do (incf i 2) (write-string (weekday-short-name (weekday date)) output)
            else if (string= control "dd" :start1 i :end1 i2)
                   do (incf i) (format output "~2,'0D" day)
            else if (char= c #\d)
                   do (princ day output)
            else if (string= control "ww" :start1 i :end1 i2)
                   do (incf i) (format output "~2,'0D" (1+ (week-of-year date)))
            else if (char= c #\w)
                   do (princ (1+ (weekday date)) output)
            else if (string= control "mmmm" :start1 i :end1 i4)
                   do (incf i 3) (write-string (month-name month) output)
            else if (string= control "mmm" :start1 i :end1 i3)
                   do (incf i 2) (write-string (month-short-name month) output)
            else if (string= control "mm" :start1 i :end1 i2)
                   do (incf i) (format output "~2,'0D" month)
            else if (char= c #\m)
                   do (princ month output)
            else if (string= control "yyyy" :start1 i :end1 i4)
                   do (incf i 3) (format output "~4,'0D" year)
            else if (string= control "yy" :start1 i :end1 i2)
                   do (incf i) (format output "~2,'0D" (mod year 100))
            else if (char= c #\y) do (princ (day-of-year date) output)
                   else if (string= control "hh" :start1 i :end1 i2)
                          do (format output "~2,'0D" hours)
            else if (char= c #\h)
                   do (princ hours output)
            else if (string= control "HH" :start1 i :end1 i2)
                   do (setf i (1+ i) am/pm t) (format output "~2,'0D" (let ((hour (mod hours 12))) (if (zerop hour) 12 hour)))
            else if (char= c #\H)
                   do (setf am/pm t) (princ (let ((hour (mod hours 12))) (if (zerop hour) 12 hour)) output)
            else if (string= control "nn" :start1 i :end1 i2)
                   do (incf i) (format output "~2,'0D" minutes)
            else if (char= c #\n)
                   do (princ minutes output)
            else if (string= control "ss" :start1 i :end1 i2)
                   do (incf i) (format output "~2,'0D" seconds)
            else if (char= c #\s)
                   do (princ seconds output)
            else
              do (write-char c output)
            finally (when am/pm (write-string (if (< hours 12) " AM" " PM") output))))))

(defun get-date-component (date component)
  "Get a value based on COMPONENT from our DATE.
date    => 1 - 31 day of month
day     => 1 - 366 day of the year
weekday => 1 - 7  weekday (i.e. 1 is Sunday, 7 is Saturday)
week    => 1 -52 week of year
month   => 1 - 12 month of year
year    => year
hour    => 0 - 23 hour of day
minute  => 0 - 59 minute of day's hour
second  => 0 - 59 second of day's minute of day's hour
unrecognised COMPONENT returns 0"
  (multiple-value-bind (seconds minutes hours day month year) (decode-universal-time (seconds date))
    (case (find date '(:date :day :weekday :week :month :year :hour :minute :second) :test #'string-equal :key #'symbol-name)
      (:date day)
      (:day (day-of-year date))
      (:weekday (1+ (weekday date)))
      (:week (1+ (week-of-year date)))
      (:month month)
      (:year year)
      (:hour hours)
      (:minute minutes)
      (:second seconds)
      (t 0))))

(defun days-in-february? (day month year)
  "February can have either 28 or 29 days"
  (if (and (= month 2) (> day 27) (< day 30))
      (+ 28 (if (leap-year-p* year) 1 0))
      day))

(defun days-in-month (date)
  "Return the number of days in the month of DATE"
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (seconds date))
    (declare (ignore second minute hour day))
    (days-in-month* month year)))

(defun days-in-month* (month year)
  "Return the number of days in the month of DATE"
  (if (/= month 2)
      (svref #(31 0 31 30 31 30 31 31 30 31 30 31) (1- month))
      (+ 28 (if (leap-year-p year) 1 0))))

(defun leap-year-p (date)
  "Is the current DATE in a leap year?"
  (leap-year-p* (if (datep date) (get-date-component date "year") date)))

(defun leap-year-p* (year)
  (and (or (zerop (mod year 400))
           (and (zerop (mod year 4)) (plusp (mod year 100))))))

(defun days-difference (date1 date2)
  "Get the number of days involved in DATE1 - DATE2"
  (floor (- (seconds date1) (seconds date2)) 86400))

(defconstant +long-month-names+
  #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
  "Array of long English month names")

(defun month-name (month)
  "Return the English full name for MONTH 1 - 12"
  (svref +long-month-names+ (1- month)))

(defun day-of-year (date)
  "Get the day number (1 - 366) of the current year"
  (multiple-value-bind (seconds minutes hours day month year) (decode-universal-time (seconds date))
    (declare (ignore seconds minutes hours))
    (+ 1 day (loop for month* from 1 below month summing (days-in-month* month* year)))))

(defun week-of-year (date)
  (1+ (floor (day-of-year date) 7)))

(defun month-by-name (str)
  "Get the month number 1 - 12 for month named STR"
  (1+ (or (position str +long-month-names+ :test #'string-equal) 0)))

(defconstant +short-month-names+
  #( "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Array of short English month names")

(defun month-short-name (month)
  "Return the English abbreviated name for MONTH 1 - 12"
  (svref +short-month-names+ (1- month)))

(defun month-by-short-name (str)
  "Get the month number 1 - 12 for month abbreviated STR"
  (if (every #'digit-char-p str)
      str
      (1+ (or (position str +short-month-names+ :test #'string-equal) 0))))

(defun weekday (date)
  "Return the weekday 0 - 6"
  (mod (1+ (nth-value 6 (decode-universal-time (seconds date) 0))) 7))

(defun weekday-name (weekday)
  "Return the English full name for WEEKDAY 0 -6"
  (svref #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday") (mod weekday 7)))

(defun weekday-short-name (weekday)
  "Return the English abbreviated name for WEEKDAY 0 - 6"
  (svref #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") (mod weekday 7)))

(defun encode-partial-universal-time (integer in-respect-of)
  "Create a new UNIVERSAL-TIME based on INTEGER but only for fields IN-RESPECT-OF"
  (multiple-value-bind (second minute hour day month year) (decode-universal-time integer 0)
    (encode-universal-time (if (member :second in-respect-of) second 0)
                           (if (member :minute in-respect-of) minute 0)
                           (if (member :hour in-respect-of) hour 0)
                           (if (member :day in-respect-of) day 1)
                           (if (member :month in-respect-of) month 1)
                           (if (member :year in-respect-of) year 2000)
                           0)))

(defun date= (date1 date2 &key (in-respect-of +date-standard-fields+))
  "Determine if DATE1 and DATE2 refer to the same IN-RESPECTdesignator(s)
e.g. (date= *date1* *date2* :in-respect'(:day :hour)) is true for 2018-05-01T15:00 and 9999-12-01T15:32"
  (= (encode-partial-universal-time (seconds date1) in-respect-of)
     (encode-partial-universal-time (seconds date2) in-respect-of)))

(defun date< (date1 date2 &key (in-respect-of +date-standard-fields+))
  "Determine if DATE1 is earlier than DATE2 for IN-RESPECTdesignator(s)
e.g. (date< *date1* *date2* :in-respect'(:day :hour))"
  (< (encode-partial-universal-time (seconds date1) in-respect-of)
     (encode-partial-universal-time (seconds date2) in-respect-of)))

(defun date<= (date1 date2 &key (in-respect-of +date-standard-fields+))
  "Determine if DATE1 is the same or earlier than DATE2 for IN-RESPECTdesignator(s)
e.g. (date= *date1* *date2* :in-respect'(:day :hour))"
  (<= (encode-partial-universal-time (seconds date1) in-respect-of)
      (encode-partial-universal-time (seconds date2) in-respect-of)))

(defun date> (date1 date2 &key (in-respect-of +date-standard-fields+))
  "Determine if DATE1 is later than DATE2 for IN-RESPECTdesignator(s)
e.g. (date= *date1* *date2* :in-respect'(:day :hour))"
  (not (date<= date1 date2 :in-respect-of in-respect-of)))

(defun date>= (date1 date2 &key (in-respect-of +date-standard-fields+))
  "Determine if DATE1 is the same or later than DATE2 for IN-RESPECTdesignator(s)
e.g. (date= *date1* *date2* :in-respect'(:day :hour))"
  (not (date< date1 date2 :in-respect-of in-respect-of)))

(defun date-between (date start end &key (in-respect-of +date-standard-fields+))
  "Determine if DATE falls between START and END IN-RESPECTdesignators(s)"
  (and (date>= date start :in-respect-of in-respect-of)
       (date<= date end :in-respect-of in-respect-of)))
