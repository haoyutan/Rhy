# 0: debug, 1: info, 2: warning, 3: normal
hy.log.level <- 3

hy.log.debug <-
function(message)
{
  if (hy.log.level < 1)
    cat(sprintf("[DEBUG] %s\n", message))
}
