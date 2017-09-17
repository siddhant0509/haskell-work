-- file ch03/Bookstore.hs

data BookInfo = Book Int String [String]
    deriving (Show)

bookId (Book id _ _) = id
bookName (Book _ name _) = name
authors (Book _ _ a) = a


type CustomerId = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerId ReviewBody
type BookRecord = (BookInfo, BookReview)



data Customer = Customer {
    customerId:: CustomerId,
    customerName:: String,
    address:: [String]
    }
    deriving (Show)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
    | CashOnDelivery
    | Invoice CustomerId
    deriving (Show, Eq)
