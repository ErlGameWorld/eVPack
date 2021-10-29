erlVPack
=====
eAarango 二进制序列化库

Build
-----
    $ rebar3 compile

# vpackVal
    * `vpackVal`: this tool can be used to validate a VPack value in a file for
      correctness. The tool expects the (binary) VPack input file it should read from 
      in its first argument. It will return status code 0 if the VPack is valid, and
      a non-0 exit code if the VPack is invalid.
    
      Further options for *vpack-validate* are:
      * `--hex`: try to turn hex-encoded input into binary vpack
    
      On Linux, *vpack-validate* supports the pseudo filename `-` for stdin.
