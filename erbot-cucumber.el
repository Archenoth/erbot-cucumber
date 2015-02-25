;;; -*- lexical-binding: t -*-
;;; erbot-cucumber.el --- An erbot Cucumber BDD testing controller

;; Copyright (C) 2015 Matthew "Archenoth" MacLean

;; Author: Archenoth <archenoth@gmail.com>
;; Version 0.0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; erbot-cucumber.el is a Cucumber BDD (http://cukes.info/) runner for
;; Emacs erbot IRC bots.

;;; Customizable variables
(defgroup erbot-cucumber nil
  "A Cucumber BDD test runner for erbots everywhere!"
  :group 'erbot)

(defcustom *erbot-cucumber-host-alist* '((local "http://127.0.0.1"
                                                :replace ((this . with-this))))
  "Defines the host alist to test, consists of names to refer to
the hosts and the actual hostname."
  :group 'erbot-cucumber
  :type '(repeat sexp))

(defcustom *erbot-cucumber-default-host* 'local
  "The default host to test if not specified"
  :group 'erbot-cucumber
  :type 'symbol)

(defcustom *erbot-cucumber-test-result-root* "http://127.0.0.1/"
  "The root where the cucumber HTML pages will be served from."
  :group 'erbot-cucumber
  :type 'string)

(defcustom *erbot-cucumber-exec* "cucumber -f html"
  "The cucumber executable or script to call and its command line
arguments in a list, and set up to accept more arguments at the
end:"
  :type 'string
  :group 'erbot-cucumber)

(defcustom *erbot-cucumber-output-path* "/var/www/cucumber/"
  "The path you want Cucumber to output it's files. (Where the
server serves from, emacs must have write permissions to this
location.)"
  :group 'erbot-cucumber
  :type 'directory)

(defcustom *erbot-cucumber-feature-location* "cucumber/features"
  "The location of the cucumber features files you wish to use."
  :group 'erbot-cucumber
  :type 'directory)

(defcustom *erbot-cucumber-reset-tree*
  (concat "cd " *erbot-cucumber-feature-location*
          " && git clean -dfx && git reset --hard && git pull")
  "The command to run to reset cucumber's tests back to their initial state."
  :group 'erbot-cucumber
  :type 'string)

(defcustom *erbot-cucumber-host-file*
  "cucumber/features/step_definitions/constants.rb"
  "The file that we will replace the old host(s) with the new
host(s) before running cucumber."
  :group 'erbot-cucumber
  :type '(file :must-match t))

(defvar *erbot-cucumber-processes* '()
"A list of processes running right now as an alist")

(defun erbot-cucumber-get-test-url (name)
  "Fetches the URL for test results for a particular host."
  (concat *erbot-cucumber-test-result-root* name ".html"))

(defun erbot-cucumber-reset-tree ()
  "Resets the files in the cucmber tree to their initial state"
  (shell-command *erbot-cucumber-reset-tree*))

(defun erbot-cucumber-make-test-sentenal (name)
  "Given a host name, this function will create an anonymous
function that can be be passed to set-process-sentinal. It will
spit out relevant information when the process ceases to exist."
  ;; Because lexical might not be a thing in this Emacs
  (let ((procc proc)
        (nickc nick)
        (tgtc tgt)
        (namec name))
    (lambda (process event)
      (erbot-reply
       (concat
        "The test for " namec " is now complete... You may view results here: "
        (erbot-cucumber-get-test-url namec)) procc nickc tgtc "" nil)
      (erbot-cucumber-remove-test namec))))

(defun erbot-replace-in-hosts-file (from to)
  "Replaces some string in the configured hosts file with another string."
  (shell-command (concat "sed -i s," from "," to ", " *erbot-cucumber-host-file*)))

(defun erbot-cucumber-begin-test (name)
  "Begins a test on the host passed in and returns a new proccess
list containing a process handle."
  (if (assoc name *erbot-cucumber-processes*)
      (concat "There is already a test for " name " ya dingus! Results: "
              (erbot-cucumber-get-test-url name))
    (let ((to-replace (getf (assoc (intern name) *erbot-cucumber-host-alist*)
                            :replace))
          (buffer (concat name "-test")))
      (erbot-cucumber-reset-tree)
      (mapc (lambda (r)
              (erbot-replace-in-hosts-file (car r) (cdr r))) to-replace)
      (let ((process (start-process buffer buffer *erbot-cucumber-exec*
                                    (concat " -o "
                                            *erbot-cucumber-output-path*
                                            name ".html ")
                                    (concat " -x "
                                            *erbot-cucumber-feature-location*))))
        (pushnew (cons name process) *erbot-cucumber-processes*)
        (set-process-sentinel process (erbot-cucumber-make-test-sentenal
                                       name))
        (concat "Started. Results will populate at "
                (erbot-cucumber-get-test-url name))))))

(defun erbot-cucumber-remove-test (name)
  "Removes a test from Cucumber's process list"
  (setq *erbot-cucumber-processes*
        (remove (assoc name *erbot-cucumber-processes*)
                *erbot-cucumber-processes*)))

(defun erbot-cucumber-stop-test (name)
  "Removes a test process from the processes alist"
  (let ((to-delete (cdr (assoc name *erbot-cucumber-processes*))))
    (if to-delete
        (progn (delete-process to-delete)
               (erbot-cucumber-remove-test name)
               (concat name " stopped..."))
      (concat "Cannot find an active process for " name))))


;;;; Bot-exposed functions

(defun fs-test (host)
  "Tests the desired host from *erbot-cucumber-hosts-alist*"
  (if (assoc host *erbot-cucumber-host-alist*)
      (erbot-cucumber-begin-test (symbol-name host))
    (concat "What's a " (symbol-name host) "?")))

(defun fs-stop (host)
  "Stops a test of a host in *erbot-cucumber-hosts-alist*"
  (erbot-cucumber-stop-test (symbol-name host)))

(defun fs-list ()
  "Lists available test hosts"
  (mapcar (lambda (host) (car host)) *erbot-cucumber-host-alist*))

(defun fs-running ()
  "Lists currently running tests"
  (mapcar (lambda (host) (car host)) *erbot-cucumber-processes*))
