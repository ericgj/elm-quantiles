module Sort exposing 
    ( Sorted
    , TaggedList(..)
    , sort
    )

{-| A simple module for guaranteeing that lists are sorted.

Eventually this may be moved to its own package if it proves useful. Right
now, it's not even exposed.

@docs sort, TaggedList, Sorted

-}


{-| Phantom type for tagging sorted lists.
-}
type Sorted
    = Sorted

{-| Phantom-taggable list
-}
type TaggedList a tag
    = TaggedList (List a)


{-| Sort list, and return a list phantom-tagged as Sorted
-}
sort : List comparable -> TaggedList comparable Sorted
sort values =
    List.sort values |> TaggedList


