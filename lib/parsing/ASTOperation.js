define([
    './_VoidToken'
  ], function(
    _VoidToken
) {
    "use strict";

    function ASTOperation(operatorToken, args, postArguments) {
        var postArgs, preArgs, splitted;
        if(postArguments) {
            preArgs = args;
            postArgs = postArguments;
        }
        else {
            splitted = this._splitArgs(operatorToken, args);
            preArgs = splitted[0];
            postArgs = splitted[1];
        }

        Object.defineProperties(this, {
            operator: {
                value: operatorToken
              , enumerable: true
            }
            , preArguments: {
                value: preArgs
              , enumerable: true
            }
            , postArguments: {
                value: postArgs
              , enumerable: true
            }
          , arguments: {
                value: preArgs.concat(postArgs)
              , enumerable: true
            }
          , ejects: {
                value: 1
              , enumerable: true
            }
        });
    }
    var _p = ASTOperation.prototype;

    /**
     * Figure out preArgs and postArgs,
     * This is for convenience when we're programmatically creating
     * ASTOperation nodes, so that we don't have to know exactly how
     * to call an operator, just the right order and amount of
     * arguments.
     * There's one undecidable situation: _VoidTokens between
     * preArgs and postArgs. If there are one ore more, the first
     * will go to preArgs and the rest will got to postArgs.
     * When printing code, we'll have to insert whitespace
     * between consecutive none-splitting tokens anyways.
     */
    _p._splitArgs = function(operatorToken, args) {
        var postArgs, preArgs, i, l, consumed;
        consumed = 0;
        i = 0;
        l = args.length;
        preArgs = [];
        while(consumed < operatorToken.preConsumes && i<l) {
            preArgs.push(args[i]);
            if(!(args[i] instanceof _VoidToken))
                consumed += 1;
            i += 1;
        }
        // Take the first whitespace after preArgs
        if(i<l && args[i] instanceof _VoidToken)
            preArgs.push(args[i]);

        // All the rest goes to postArgs.
        postArgs = args.slice(preArgs.length);

        return [preArgs, postArgs];
    };

    return ASTOperation;
});
