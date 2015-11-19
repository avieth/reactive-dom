# Issues

1. Values for input elements. When an input is rendered, the value is not
   set. It's possible to keep its value in-sync *after* rendering, via
   virtual reactimates, but this is not enough. One possible solution is to
   add properties, in addition to attributes and style and children, to the
   definition of a virtual element, and apply all properties at render time.
   Seems too broad, though.
