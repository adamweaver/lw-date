# Cromulent date/time parser 
This is a dead-simple date parser, it uses Edi's `CL-PPCRE` under the hood via a read macro
`#~m//` 

## Main API

``` common-lisp
(DATE:NOW)
```

``` common-lisp
(DATE:MAKE-DATE string)
```

``` common-lisp
(DATE:FORMAT-DATE date &optional format)
```

``` common-lisp
(DATE:DATE+ date &key year month day hour minute second)
```

``` common-lisp
DATE:DATEP DATE:DATE< DATE:DATE<= DATE:DATE= DATE:DATE> DATE:DATE>=
```

``` common-lisp
;; FORMATS
 DATE:+ISO8601+ DATE:+NATURAL+ DATE:+YYYY-MM-DD+ DATE:+RFC2822+ DATE:+RFC3501-SEARCH+ DATE:+HTTP/1.1+ DATE:+PDF+
```
