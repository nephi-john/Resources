%AppData%

\Local\BraveSoftware\Brave-Browser\User Data\Default

http://2016.padjo.org/tutorials/sqlite-your-browser-history/

https://sqlitestudio.pl/

=(C2-DATE(1970,1,1))*86400
=(((A1/60)/60)/24)+DATE(1970,1,1)

https://www.epochconverter.com/
https://community.brave.com/t/not-solved-translating-visit-time-in-history-to-human-readable-format/182189/3

https://forensicswiki.xyz/page/Google_Chrome#visits.visit_time

visits.visit_time

The visits.visit_time is in (the number of) microseconds since January 1, 1601 UTC

=((([@[last_visit_time]]/1000000-11644473600)/60/60)-4)/24+DATE(1970,1,1)

-11644473600 ... January 1, 1601
-4 ... EST time zone from GMT





https://techcommunity.microsoft.com/t5/excel/does-anyone-know-how-to-hide-the-login-name-in-the-upper-right/m-p/25221

You can temporarily change the displayed username by editing the registry. For Office 2013, navigate to 

HKEY_CURRENT_USER\Software\Microsoft\Office\15.0\Common\Identity\Identities in RegEdit and edit the key "FriendlyName" to whatever you want.

HKEY_CURRENT_USER\Software\Microsoft\Office\16.0\Common\Identity\Identities\xxxyyyzzz

PIPE DELIMITED ...
> Region Settings > Additional Date, Time Settings > Region > Additional Settings > List Separator > | ... Apply




