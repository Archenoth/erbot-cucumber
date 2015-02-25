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
  "The configuration for erbot-cucumber, a Cucumber BDD test
runner for erbots everywhere!"
  :group 'erbot)

(defcustom *erbot-cucumber-host-alist*
  '((local "http://127.0.0.1"
           :replace (("http://somehost" . "https://the-right-host"))))
  "Defines the host alist the bot will recognize for tests...

Each alist entry contains an adress for the host, and optionally,
a list of dotted pairs that we will replace the first term for
the second in the configured *erbot-cucumber-host-file*

Eg: If you have a variable pointing to your host in Cucumber, and
it is by default, pointing to your staging staging server at
\"http://staging\" and you wanted to add a test for your
production server, which is located at \"https://production\",
you can do that by adding the following alist entry:

(prod \"https://production\"
  :replace ((\"http://staging\" . \"https://production\")))"
  :group 'erbot-cucumber
  :type '(repeat sexp))

(defcustom *erbot-cucumber-default-host* 'local
  "The default host to test if not specified... So if you invoke
  \",test\" in the IRC room without speciying what you want to
  test, it will test the host indicated by this variable
  instead."
  :group 'erbot-cucumber
  :type 'symbol)

(defcustom *erbot-cucumber-test-result-root* "http://127.0.0.1/"
  "The root where the cucumber HTML pages will be served from.

So, if you had a server serving files from \"http://127.0.0.1\"
using the folder cucmber outputs its files to, you do not need to
change this variable... But if they appear in a place like
\"http://127.0.0.1/cucumber\", you would need to configure
it... The runner isn't that smart."
  :group 'erbot-cucumber
  :type 'string)

(defcustom *erbot-cucumber-exec* "cucumber -f html"
  "The cucumber executable or script to call along with any
command line arguments you wish to pass to it... Default is
\"cucumber -f -h\" because we likely want to output HTML to the
file we link to in the IRC room...

In actuality, this can be anything that accepts cucumber's
command line arguments, including shell scripts that perform some
other task like running xpra so you can use the bot on a headless
server."
  :type 'string
  :group 'erbot-cucumber)

(defcustom *erbot-cucumber-output-path* "/var/www/cucumber/"
  "The path you want Cucumber to output it's files. (Where the
server serves from, emacs must have write permissions to this
location.)

This will be the place that Cucumber puts the results it creates,
and should also be accessible by looking at the configured
*erbot-cucumber-test-result-root*"
  :group 'erbot-cucumber
  :type 'directory)

(defcustom *erbot-cucumber-feature-location* "cucumber/features"
  "The location of the Cucumber features files you wish the bot
to use."
  :group 'erbot-cucumber
  :type 'directory)

(defcustom *erbot-cucumber-reset-tree*
  (concat "cd " *erbot-cucumber-feature-location*
          " && git clean -dfx && git reset --hard && git pull")
  "These command(s) will be run to reset Cucumber's tests back to
their initial state, resetting the *erbot-cucumber-host-file*
back to its original state, and cleaning the test tree.

If you have your Cucumber tests in a git repo, the above is an
example of what you would need to do before each test. (And
very-well may work out of the box.)"
  :group 'erbot-cucumber
  :type 'string)

(defcustom *erbot-cucumber-host-file*
  "cucumber/features/step_definitions/constants.rb"
  "The file that contains the the configuration for the host to
test. we will replace the old host(s) with the new host(s) before
running cucumber. (As per *erbot-cucumber-host-alist*)"
  :group 'erbot-cucumber
  :type '(file :must-match t))

(defvar *erbot-cucumber-processes* '()
"A list of processes running right now as an alist")

(defun erbot-cucumber-get-test-url (name)
  "Fetches the URL where the test results for a particular host
will populate."
  (concat *erbot-cucumber-test-result-root* name ".html"))

(defun erbot-cucumber-reset-tree ()
  "Resets the files in the cucmber tree to their initial state."
  (shell-command *erbot-cucumber-reset-tree*))

(defun erbot-cucumber-make-test-sentenal (name)
  "Given a host name, this function will create an anonymous
function that can be be passed to set-process-sentinal. It will
spit out relevant information to the room the bot was invoked
when the test either completes, or fails miserably. (When the
process ends)"
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
  (shell-command (concat "sed -i s," from "," to ", "
                         *erbot-cucumber-host-file*)))

(defun erbot-cucumber-begin-test (name)
  "Begins a test on the host passed in and returns an
IRC-friendly message indiciating how it went."
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
  "Removes a test from our Cucumber process list"
  (setq *erbot-cucumber-processes*
        (remove (assoc name *erbot-cucumber-processes*)
                *erbot-cucumber-processes*)))

(defun erbot-cucumber-stop-test (name)
  "Stops a running Cucumber test, removes the test process from
the processes alist, then returns an IRC-friendly string telling
us how it went."
  (let ((to-delete (cdr (assoc name *erbot-cucumber-processes*))))
    (if to-delete
        (progn (delete-process to-delete)
               (erbot-cucumber-remove-test name)
               (concat name " stopped..."))
      (concat "Cannot find an active process for " name))))


;;;; Bot-exposed functions

(defun fs-test (&optional host)
  "Tests the desired host from *erbot-cucumber-hosts-alist* and
returns an IRC-friendly string telling us how it went."
  (if (null host)
      (erbot-cucumber-begin-test *erbot-cucumber-default-host*)
    (if (assoc host *erbot-cucumber-host-alist*)
        (erbot-cucumber-begin-test (symbol-name host))
      (concat "What's a " (symbol-name host) "?"))))

(defun fs-stop (host)
  "Stops a test of a host in *erbot-cucumber-hosts-alist* and
returns an IRC-friendly string telling us how it went."
  (erbot-cucumber-stop-test (symbol-name host)))

(defun fs-list ()
  "Lists available test hosts"
  (mapcar (lambda (host) (car host)) *erbot-cucumber-host-alist*))

(defun fs-running ()
  "Lists currently running tests"
  (mapcar (lambda (host) (car host)) *erbot-cucumber-processes*))
