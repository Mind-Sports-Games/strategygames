#!/bin/sh

# checks if locally staged changes are 
# formatted properly. Ignores non-staged
# changes.
# Intended as git pre-commit hook

_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
DIR=$( echo $_DIR | sed 's/\/.git\/hooks$//' )

echo ""
echo "Running pre-commit hook ... (you can omit this with --no-verify, but don't)"
git diff --quiet
hadNoNonStagedChanges=$?

if ! [ $hadNoNonStagedChanges -eq 0 ]
then
    echo "* Stashing non-staged changes"
    git stash --keep-index -u > /dev/null
else
    echo "* You had no non-staged changes"
fi

echo "* Compiling/formatting staged changes"
sbt compile > /dev/null
compiles=$?

echo "* Compiles?"

if [ $compiles -eq 0 ]
then
    echo "* Yes!"
		
    echo "* Properly formatted?"
    cd $DIR/; sbt scalafmt
    git diff --quiet
    formatted=$?

    if [ $formatted -eq 0 ]
    then
    echo "* Yes!"
    else
        echo "* No!"
        echo "The following files need formatting (in stage or commited):"
        git diff --name-only
        echo "* Undoing formatting"
        git stash --keep-index > /dev/null
        git stash drop > /dev/null
    fi
else
    echo "* No!"
fi

if ! [ $hadNoNonStagedChanges -eq 0 ]
then
    echo "* Scheduling stash pop of previously stashed non-staged changes for 1 second after commit"
    sleep 1 && git stash pop --index > /dev/null & # sleep and &, otherwise commit fails when this leads to a merge conflict
fi

if [ $compiles -eq 0 ] && [ $formatted -eq 0 ]
then
    echo "... done. Proceeding with commit."
    exit 0
elif [ $compiles -eq 0 ]
then
    echo "... done."
    echo "CANCELLING commit due to NON-FORMATTED CODE."
    exit 1
else
    echo "... done."
    echo "CANCELLING commit due to COMPILE ERROR."
    exit 2
fi
