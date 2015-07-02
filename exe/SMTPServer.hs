import DnoList.SMTPServer
import DnoList.Wrapper

main :: IO ()
main = wrapMain runSmtpServer
