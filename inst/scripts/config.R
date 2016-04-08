cluster.functions = makeClusterFunctionsSGE(system.file("scripts/sge.tmpl", package="MethylAid"))
mail.start = "first+last"
mail.done = "first+last"
mail.error = "all"
mail.from = "<my@sender.adress.com>"
mail.to = "<my@recipient.adress.com>"
mail.control = list(smtpServer="my.mail.server.com")
debug = FALSE
