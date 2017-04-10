# CL-RANGE

A datastructure designed to maintain a hierarchy of 'ranges' that map onto text in a buffer.  The ranges may nest inside one another but may not overlap (although they _may_ be exactly the same size as parent ranges)

Zero-width ranges present a special challenge.  As implemented, a zero-width range will be found instead of the next range.  This makes it possible to create a zero range and address it...

