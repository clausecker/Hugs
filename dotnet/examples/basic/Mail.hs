--
-- SmtpMail.Send example
--
module Mail where

import DotNet

sendMail :: String
	 -> String
	 -> String
	 -> String
	 -> IO ()
sendMail fromAddr toAddr subj body = do
  ()    <- invokeStatic "System.Web.Mail.SmtpMail"
  			"Send" 
  		        ( fromAddr
			, toAddr
			, subj
			, body -- "<html><body>Greetings from <em>Hugs98.NET</em></body></html>"
			)
  putStrLn "mail sent"
