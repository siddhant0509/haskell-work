-- file ch03/Maybe.hs

--data Maybe a = Just a | Nothing

fromMaybe:: a -> Maybe a -> a
fromMaybe defValue wrapped = case wrapped of
    Nothing -> defValue
    Just x -> x


