(defmodule llfe
  (export all))

(include-lib "kernel/include/file.hrl")

(include-lib "clj/include/compose.lfe")

(defun collect-to-eol (input)
  (case (lists:splitwith #'not-newline?/1 input)
    (`#(,line [10 . ,rest]) `#(,line ,rest))
    (`#(,line ,rest)        `#(,line ,rest))))

(defun not-newline?
  ([10] 'false)
  ([_] 'true))

(defun collect-to-fence (input) (collect-to-fence input ""))

(defun collect-to-fence
  ([""                        acc] `#(,(lists:reverse acc) ""))
  ;; 10 = \n
  ([`(10 #\` #\` #\` . ,rest) acc] `#(,(lists:reverse acc) ,rest))
  ([`(,c . ,rest)             acc] (collect-to-fence rest (cons c acc))))

(defun all-code (input) (all-code input ""))

(defun all-code
  ([""                 acc] (lists:reverse acc))
  ([`(10 #\` #\` #\` . ,rest) acc]
   (let* ((`#(,attr ,rest1)  (collect-to-eol rest))
          (`#(match [,name]) (match-name attr))
          (`#(,code ,rest2)  (collect-to-fence rest1)))
     (all-code rest2 `[#(,name ,code) . ,acc])))
  ([`(,_ . ,rest)             acc] (all-code rest acc)))

(defun concat-sections (sections)
  (flet ((join-section (key)
           `#(,key ,(unlines (proplists:get_all_values key sections)))))
    (lists:map #'join-section/1 (proplists:get_keys sections))))

(defun split-section (line)
  (case (collect-to-replacement-open line)
    (`#(,_ "") 'nil)
    (`#(,prefix ,rest)
     (let ((`#(,padded-name ,suffix) (collect-to-replacement-close rest)))
       `#(,(string:strip padded-name) ,prefix ,suffix)))))

(defun collect-to-replacement-open (line)
  (collect-to-replacement-open line []))

(defun collect-to-replacement-open
  (["" acc]
   `#(,(lists:reverse acc) ""))
  ([`(#\\ #\< #\< . ,rest) acc]
   (collect-to-replacement-open rest (++ "<<\\" acc)))
  ([`(#\< #\< . ,rest) acc]
   `#(,(lists:reverse acc) ,rest))
  ([`(,c . ,rest) acc]
   (collect-to-replacement-open rest (cons c acc))))

(defun collect-to-replacement-close (input)
  (collect-to-replacement-close input []))

(defun collect-to-replacement-close
  ([""                 acc] `#(,(lists:reverse acc) ""))
  ([`(#\> #\> . ,rest) acc] `#(,(lists:reverse acc) ,rest))
  ([`(,c . ,rest)      acc] (collect-to-replacement-close rest (cons c acc))))

(defun expand-sections (code sections) (expand-sections code sections []))

(defun expand-sections
  ([""   _sections acc] (unlines (lists:reverse acc)))
  ([code sections  acc]
   (let ((`#(,line ,rest) (collect-to-eol code)))
     (case (split-section line)
       ('nil (expand-sections rest sections (cons line acc)))
       (`#(,name ,prefix ,suffix)
        (case (proplists:get_value name sections)
          ('undefined
           (io:fwrite "Warning: code section named ~p not found.~n" `[,name])
           (expand-sections rest sections (cons (++ prefix suffix) acc)))
          (code-to-insert
           (-> (lists:map (lambda (x) (++ prefix x suffix)) (lines code-to-insert))
               (unlines)
               (cons acc)
               (->> (expand-sections rest sections))))))))))

(defun expand-all-sections (sections)
  (lists:map 
    (match-lambda
      ([`#(,name ,code)]
       `#(,name ,(expand-sections code sections))))
    sections))

(defun unescape (code) (re:replace code "\\\\<<" "<<" '[global #(return list)]))

(defun unescape-sections (sections)
  (lists:map
    (match-lambda ([`#(,name ,code)] `#(,name ,(unescape code))))
    sections))

(defun file-sections (sections)
  (lists:filtermap
    (match-lambda
      ([`#(,(= `(#\f #\i #\l #\e #\: . ,_) name) ,code)]
       `#(true #(,name ,code)))
      ([_] 'false))
    sections))

(defun file-name (base-dir filename)
  (filename:nativename (filename:absname_join base-dir filename)))

(defun write-file (base-dir filename contents)
  (let ((filename* (file-name base-dir filename)))
    (case (file:write_file filename* contents)
      ('ok filename*)
      (`#(error ,reason)
       (io:format (++ "Error: Failed to write file (~s): ~s. "
                      "(LLFE doesn't create directories, so you may need to "
                      "create one.)~n")
                  `[,filename* ,reason])))))

(defun process-file (filename)
  (let* ((base-dir          (filename:dirname filename))
         (concatenated-code (concat-sections (all-code (read-file filename))))
         (expanded-code     (-> concatenated-code
                                (expand-all-sections)
                                (expand-all-sections)
                                (expand-all-sections)
                                (expand-all-sections)))
         (files (file-sections (unescape-sections expanded-code))))
    (write-file-sections base-dir files)))

(defun write-file-sections (base-dir files)
  (-> (match-lambda
        ([`#((#\f #\i #\l #\e #\: . ,filename) ,contents)]
         (write-file base-dir filename contents)))
      (lists:map files)
      (lists:reverse)))

(defun process-files (files)
  (lists:reverse (lists:flatmap #'process-file/1 files)))

(defun file-modified-time (filename)
  (let ((`#(ok ,info) (file:read_file_info filename)))
    (file_info-mtime info)))

(defun modified-times (files)
  (lists:map (lambda (file) `#(,file ,(file-modified-time file))) files))

(defun watch (files f) (watch files f []))

(defun watch (files f state)
  (let* ((modified-times (modified-times (existing-files files)))
         (changed-files  (changed-files modified-times state)))
    (if (> (length changed-files) 0)
      (apply f `[,changed-files])
      'noop)
    (timer:sleep (timer:seconds 1))
    (watch files f modified-times)))

(defun changed-files (a b)
  (-> (lambda (x) (=/= (proplists:get_value x a) (proplists:get_value x b)))
      (lists:filter (proplists:get_keys a))))

(defun existing-files (files) (lsits:filter #'filelib:is_file/1 files))

(defun read-file (filename)
  (case (file:read_file filename)
    (`#(ok ,binary) (binary_to_list binary))
    (`#(error ,reason)
     (io:format "Failed to read file (~s): ~s~n" `[,filename ,reason])
     (error `#(read_file ,filename ,reason)))))

(defun print-sections (sections)
  (lists:foreach
   (lambda (name-code)
     (io:format "~s~n-----~n~s~n-----~n~n"
                (tuple_to_list name-code)))
   sections))

(defun print-code (filename)
  (print-sections (all-code (read-file filename))))

(defun print-unindented-code (filename)
  (print-sections (all-code (read-file filename))))

(defun print-concatenated-code (filename)
  (print-sections (concat-sections (all-code (read-file filename)))))

(defun print-expanded-code (filename)
  (-> (all-code (read-file filename))
      (concat-sections)
      (expand-all-sections)
      (print-sections)))

(defun print-unescaped-code (filename)
  (-> (all-code (read-file filename))
      (concat-sections)
      (expand-all-sections)
      (unescape-sections)
      (file-sections)
      (print-sections)))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun match-name (input)
  (re:run input "name=\"(?<name>[^\"]+)\"" '[#(capture [name] list)]))

(defun lines (input) (re:split input "\n" '[#(return list)]))

(defun unlines (input) (string:join input "\n"))
