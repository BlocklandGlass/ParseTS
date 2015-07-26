returntrueif(isTrue(true), true, "false");

function isTrue(%cond) {
    return %cond == true;
}

function returnif(%cond, %ifTrue, %ifFalse) {
    if (%cond) {
        return %ifTrue; // It's true!
    } else {
        return %ifFalse; // Or not...
    }
}
