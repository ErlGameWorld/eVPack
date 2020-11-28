-type vpack() :: binary() | iodata().
-type vpOpt() :: pos_integer().
-export_type([vpack/0, vpOpt/0]).

-define(vpObjNcNs, 0).        %% 不压缩编码 不排序Key
-define(vpObjNcYs, 1).        %% 不压缩编码 排序Key
-define(vpObjYc, 2).          %% 压缩编码

-define(vpArrNc, 0).          %% 不压缩编码
-define(vpArrYc, 1).          %% 压缩编码

-define(VpDefObjOpt, 0).         %% 默认选项 不排序Obj key  Obj不压缩
-define(VpDefArrOpt, 0).         %% 默认选项 Arr不压缩

-define(VpAllOpts(Arr, Obj), Obj bsl 1 bor Arr).      %% 拼装Obj 与 Arr选项
-define(VpObjOpts(VpAllOpts), VpAllOpts bsr 1).        %% 获取Obj选项
-define(VpArrOpts(VpAllOpts), VpAllOpts band 1).       %% 获取Arr选项

-define(VpBinaryCopyRatio, 1.2).