define([
    './Expression'
],
function (
    Expression
) {
    "use strict";
    // this method is the single point uses to inject the Property-Language
    // interpreter into the CPS engine. It's used by Atem-Cps/cpsTools
    function initializePropertyValue(name, propertyValue) {
        propertyValue.initialize(name, Expression.factory);
    }

    return initializePropertyValue;
});
