# Revision history for takedouble

## 0.0.1.0 -- 2022-06-22

* First version. Released on an unsuspecting world.
  In this first episode, files are lazily compared by filesize, then by first and last 4 kilobytes, and then by entire file contents.
  Duplicates are reported as a list of results.

## 0.0.2.0 -- 2022-06-26

* Glob patterns for ignoring files or directories.
  In normal use, files in the .git directory were showing up way too often.
  Thanks to the contributors for adding this functionality.
