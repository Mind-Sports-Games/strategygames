# Steps for pre-commit git hook to check for compile and format

1) This repository uses [scalafmt](https://scalameta.org/scalafmt/). Please ensure it is installed.

2) path to the hook folder
> `cd hook`

3) run the install script this will create two files in `.git/hooks` with execute permissions
> `./pre-commit-hook-install.sh`

4) You are all set up and good to go. If the commit fails due to formatting make sure you run `sbt scalafmt`!

If there are changes to the pre-commit script you will need to run the install again

Links that this was followed from:
- https://medium.com/zyseme-technology/code-formatting-scalafmt-and-the-git-pre-commit-hook-3de71d099514
- https://gist.github.com/cvogt/2676ed6c6d1abafa3d6a

