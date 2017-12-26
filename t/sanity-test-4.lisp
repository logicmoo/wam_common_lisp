#|

user:f_u_distance_from_origin(Point, From_origin_Ret) :-
        nop(global_env(Global_env_Ret)),
        Env=[bv(u_point, Point)|Global_env_Ret],
        get_var(Env, u_point, Point5),
        f_u_point_x(Point5, X),
        get_var(Env, u_point, Point6),
        f_u_point_y(Point6, Y),
        get_var(Env, u_point, Point7),
        f_u_point_z(Point7, Z),
        Env8=[bv(u_x, X), bv(u_y, Y), bv(u_z, Z)|Env],
        get_var(Env8, u_x, X12),
        *(X12, X12, _20732890),
        get_var(Env8, u_y, Y13),
        *(Y13, Y13, _20732906),
        +(_20732890, _20732906, _20732914),
        get_var(Env8, u_z, Z14),
        *(Z14, Z14, _20732930),
        +(_20732914, _20732930, Sqrt_Param),
        cl_sqrt(Sqrt_Param, Sqrt_Ret),
        Sqrt_Ret=From_origin_Ret.

user:f_u_point_x(Obj, Point_x_Ret) :-
        nop(global_env(Global_env_Ret)),
        Env=[bv(u_obj, Obj)|Global_env_Ret],
        get_var(Env, u_obj, Obj5),
        get_opv(Obj5, kw_x, X),
        X=Point_x_Ret.

f_u_point_x(A, F) :-
        nop(global_env(B)),
        C=[bv(u_obj, A)|B],
        get_var(C, u_obj, D),
        get_opv(D, kw_x, E),
        E=F.

user:f_u_distance_from_origin(Point, From_origin_Ret) :-
        nop(global_env(Global_env_Ret)),
        Env=[bv(u_point, Point)|Global_env_Ret],
        f_u_point_x(Point_x_Param, X),
        f_u_point_y(Point_x_Param, Y),
        f_u_point_z(Point_x_Param, Z),
        Env5=[bv(u_x, X), bv(u_y, Y), bv(u_z, Z)|Env],
        get_var(Env5, u_x, X9),
        *(X9, X9, _23601668),
        get_var(Env5, u_y, Y10),
        *(Y10, Y10, _23601684),
        +(_23601668, _23601684, _23601692),
        get_var(Env5, u_z, Z11),
        *(Z11, Z11, _23601708),
        +(_23601692, _23601708, Sqrt_Param),
        cl_sqrt(Sqrt_Param, Sqrt_Ret),
        Sqrt_Ret=From_origin_Ret.

|#
(defun a () 1)

