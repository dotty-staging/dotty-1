package scala.quoted

/** Current Quotes in scope */
def qctx(using qctx: Quotes): qctx.type = qctx
