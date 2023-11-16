-ifndef(eVPack_H_).
-define(eVPack_H_, true).

-type vpack() :: binary() | iodata().
-type vpOpt() :: pos_integer().
-export_type([vpack/0, vpOpt/0]).


-define(VpObjNcYs, 0).                                   %% 不压缩编码 排序Key
-define(VpObjYc, 1).                                     %% 压缩编码

-define(VpArrNc, 0).                                     %% 不压缩编码
-define(VpArrYc, 1).                                     %% 压缩编码

-define(VpArrDef, ?VpArrNc).                             %% 默认选项 Arr不压缩排序
-define(VpObjDef, ?VpObjNcYs).                           %% 默认选项 排序Obj key  Obj不压缩

-define(blob, blob).                                     %% 二进制tag 标记

-define(VpBinaryCopyRatio, 1.2).

-endif.