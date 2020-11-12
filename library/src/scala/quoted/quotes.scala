package scala.quoted

/** Current Quotes in scope */
def quotes(using quotes: Quotes): quotes.type = quotes
