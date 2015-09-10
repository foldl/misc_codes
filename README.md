# misc codes

A collection of miscellaneous codes.

## millionaires.erl

An implementation of Yao's solution to the [Millionaires' Problem](http://en.wikipedia.org/wiki/Yao's_Millionaires'_Problem).

Usage:

1. Start two nodes;
1. Synchronize their cookies, for example by using erlang:set\_cookie(node(), 'your cookie');
1. On the node that has more computing power (this is named alice): millionaires:connect(node name of bob);
1. Input wealth on each node by millionaires:input(). (io:get\_password() is used here)

Note:

Supported weatlth range is (1, MAX\_VALUE). Set MAX\_VALUE up to 50000, it could be solved within half an hour.

Acknowledge: Prime generation code (c) Joe Armstrong with some modifications.

## ftpgo.erl

Sometimes, we need to use FTP download/upload as a measurement of network speed. 
In these cases, we don't care about file content, so why bother to save them to disks?

ftpgo.erl is a pseudo-ftp client that features no file I/O, no need of care,  and "unlimited" number of threads. 
All threads are downloading the same file, hoping to reduce disk I/O load on the server.

Compatible with FileZilla Server, Serv-U. (Others are not tested)

## vftpd.erl

Then, why bother to use a *true* FTP server?

vftp.erl is a pseudo-ftp server that features no file I/O, zero configuration, highly concurrent. 

Acknowledge: Based on ftpd.erl by [tony](https://github.com/tonyrog)

## ftpd.erl

This is [tony's](https://github.com/tonyrog) ftpd.erl with some updates to fix warnings, and UTF-8 is used in 
file name (see RFC 2640 "Internationalization of the File Transfer Protocol").

