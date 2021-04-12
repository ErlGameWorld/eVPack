-type vpack() :: binary() | iodata().
-type vpOpt() :: pos_integer().
-export_type([vpack/0, vpOpt/0]).


-define(VpObjNcYs, 0).        %% 不压缩编码 排序Key
-define(VpObjYc, 1).          %% 压缩编码
-define(VpObjNcNs, 2).        %% 不压缩编码 不排序Key 暂时不要用此选项去编码object

-define(VpArrNc, 0).          %% 不压缩编码
-define(VpArrYc, 1).          %% 压缩编码

-define(VpObjDef, 0).         %% 默认选项 排序Obj key  Obj不压缩
-define(VpArrDef, 0).         %% 默认选项 Arr不压缩排序

-define(VpAllOpts(Arr, Obj), Obj bsl 1 bor Arr).      %% 拼装Obj 与 Arr选项
-define(VpObjOpts(VpAllOpts), VpAllOpts bsr 1).        %% 获取Obj选项
-define(VpArrOpts(VpAllOpts), VpAllOpts band 1).       %% 获取Arr选项

-define(blob, blob).          %% 二进制tag 标记

-define(VpBinaryCopyRatio, 1.2).