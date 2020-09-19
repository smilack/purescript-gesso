"use strict"

exports._unsafeLogAnything = function(a) {
   return function() {
      console.log(a);
   }
}
