textseg
=======

Use `cabal configure`, `cabal build` to create an executable in the `dist` directory. You may need to `cabal install` some dependencies.

The TopicTiling segmentation algorithm requires the "lda" executable from [GibbsLDA++](http://gibbslda.sourceforge.net/) to be in your $PATH. In fact, a slightly [modified version](https://github.com/gmaslov/gibbslda-mode) (implementing the mode method for topic assignment) is required.

The "NLTK" segmentation algorithm (using the TextTiling implementation in NLTK) requires, naturally, that [NLTK](http://nltk.org/) be installed and available to Python.

TODO: write the rest of this readme

