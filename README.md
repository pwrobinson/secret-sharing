Implementation of an (`m`,`n`)-threshold secret sharing scheme.
A given ByteString `b` (the secret) is split into `n` shares,
and any `m` shares are sufficient to reconstruct `b`.
The scheme preserves information-theoretic perfect secrecy in the sense that the knowledge of up
to `m-1` shares does not reveal any information about the secret `b`.

*Example in GHCi:*
Suppose that you want to split the string "my secret data" into n=5 shares such that
at least m=3 shares are necessary to reconstruct the secret.

> $ :m + Data.ByteString.Lazy.Char8 Crypto.SecretSharing
> $ let secret = pack "my secret message!"
> $ shares <- encode 3 5 secret
> $ mapM_ (Prelude.putStrLn . show) shares -- each share should be deposited at a different site.
> (1,"\134\168\154\SUBV\248\CAN:\250y<\GS\EOT*\t\222_\140")
> (2,"\225\206\241\136\SUBse\199r\169\162\131D4\179P\210x")
> (3,"~\238%\192\174\206\\\f\214\173\162\148\&3\139_\183\193\235")
> (4,"Z\b0\188\DC2\f\247\f,\136\&6S\209\&5\n\FS,\223")
> (5,"x\EM\CAN\DELI*<\193q7d\192!/\183v\DC3T")
> $ let shares' = Prelude.drop 2 shares
> $ decode shares'
> "my secret message!"

The mathematics behind the secret sharing scheme is described in:
\"/How to share a secret/.\" by Adi Shamir.
In Communications of the ACM 22 (11): 612–613, 1979.
