# `thunk-spy` Haskell package

The tiny package in this repository is an exploration of an idea I had for how to use `unsafePerformIO` in a way that is "obviously" safe:
the only `IO` actions embedded in pure thunks are innocuous, merely adding a message to a queue.
This queue is then observed elsewhere, in the monad `IO`.
That's it; I just wanted a little library that codified that idea without imposing too many opinions about how it should be used.
EG I've still forced the user to choose between `seq` and `pseq` to embed their instrumentation into their pure code;
that aspect is going to be tricky no matter how you go about it, so I wanted the signatures to make it clear that I'm in no way trying to solve that question for you.

Also, this is my first deployment of weak pointers.
I think it adds some interesting expressiveness (eg you can be notified when a certain subset of thunks are all either forced or GC'd),
but we'll need to come up with some good stress tests before I'm confident about it.
