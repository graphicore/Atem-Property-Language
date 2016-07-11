define([
  ], function(
) {
    "use strict";

    function ASTGrouping(groupingToken, nodes) {
        Object.defineProperties(this, {
            groupingToken: {
                value: groupingToken
              , enumerable: true
            }
          , nodes: {
                value: nodes
              , enumerable: true
            }
          , ejects: {
                value: 1
              , enumerable: true
            }
        });
    }

    return ASTGrouping;
});
