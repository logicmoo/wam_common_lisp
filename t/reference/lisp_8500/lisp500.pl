/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Lisprolog -- Interpreter for a simple Lisp. Written in Prolog.
    Written Nov. 26th, 2006 by Markus Triska (triska@gmx.at).
    Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:-module(moo_ext_lisp_triska,[codelist_to_forms/2]).
% #include_<ctype.h>
% #include_<errno.h>
% #include_<fcntl.h>
% #include_<math.h>
% #include_<setjmp.h>
% #include_<stdarg.h>
% #include_<stdio.h>
% #include_<stdlib.h>
% #include_<string.h>
% #ifdef_MACH_
% #define_setjmp(e)sigsetjmp(e,0)
% #define_longjmp_siglongjmp
% #endif
% #ifdef_WIN32
% #include_<windows.h>
% #defineXdeclspec(dllexport)
% #else
% #defineX
% #endif
% #ifndef_WIN32
% #include_<sys/time.h>
% #include_<unistd.h>
% #include_<dlfcn.h>
% #include_<sys/utsname.h>
% #endif
% #typedef_int_lval;
lisp500_fn([lval_PTR_o2c(lval_o)]):-true.
%- {
%=    return (lval_PTR)(o-1);
%- }
lisp500_fn([lval_c2o(lval_PTR_c)]):-true.
%- {
%=    return (lval)c+1;
%- }
lisp500_fn([int_cp(lval_o)]):-true.
%- {
%=    return (o &3)==1;
%- }
lisp500_fn([lval_PTR_o2a(lval_o)]):-true.
%- {
%=    return (lval_PTR)(o-2);
%- }
lisp500_fn([lval_a2o(lval_PTR_a)]):-true.
%- {
%=    return (lval)a+2;
%- }
lisp500_fn([int_ap(lval_o)]):-true.
%- {
%=    return (o &3)==2;
%- }
lisp500_fn([lval_PTR_o2s(lval_o)]):-true.
%- {
%=    return (lval_PTR)(o-3);
%- }
lisp500_fn([char_PTR_o2z(lval_o)]):-true.
%- {
%=    return (char_PTR)(o-3+2PTR_sizeof(lval));
%- }
lisp500_fn([lval_s2o(lval_PTR_s)]):-true.
%- {
%=    return (lval)s+3;
%- }
lisp500_fn([int_sp(lval_o)]):-true.
%- {
%=    return (o &3)==3;
%- }
lisp500_fn([struct_symbol_init{
%=    const_char_PTR_name;
%=    lval(PTR_fun)();
%=    int_argc;
%=    lval(PTR_setfun)();
%=    int_setargc;
%=    lval_sym;
%- };
% #extern_struct_symbol_init_symi_ARRAY;
% #define_TRUE_symi[1].sym
% #defineTg[-2]
% #defineUg[-3]
% #defineVg[-4]
% #defineWg[-5]
% #define_NF(n)lval_PTR_g;g=f+n+3;f[1]=0;g[-1]=(n<<5)|16;PTR_g=PTR_f;
% #defineEPTRf
% #define_NE_PTRg
lisp500_fn([lval_car(lval_c)]):-true.
%- {
%=    return (c &3)==1?o2c(c)[0]_:0;
%- }
lisp500_fn([lval_cdr(lval_c)]):-true.
%- {
%=    return (c &3)==1?o2c(c)[1]_:0;
%- }
lisp500_fn([lval_caar(lval_c)]):-true.
%- {
%=    return car(car(c));
%- }
lisp500_fn([lval_lread(lval_PTR)]):-true.
%- {
%- }
lisp500_fn([lval_cdar(lval_c)]):-true.
%- {
%=    return cdr(car(c));
%- }
lisp500_fn([lval_evca(lval_PTR,lval)]):-true.
%- {
%- }
lisp500_fn([lval_cadr(lval_c)]):-true.
%- {
%=    return car(cdr(c));
%- }
lisp500_fn([int_dbgr(lval_PTR,int,lval,lval_PTR)]):-true.
%- {
%- }
lisp500_fn([lval_cddr(lval_c)]):-true.
%- {
%=    return cdr(cdr(c));
%- }
lisp500_fn([void_print(lval)]):-true.
%- {
%- }
lisp500_fn([lval_set_car(lval_c,lval_val)]):-true.
%- {
%=    return o2c(c)[0]=val;
%- }
lisp500_fn([lval_set_cdr(lval_c,lval_val)]):-true.
%- {
%=    return o2c(c)[1]=val;
%- }
lisp500_fn([lval_PTR_binding(lval_PTR_f,lval_sym,int_type,int_PTR_macro)]):-true.
%- {
%=    lval_env;
%=  st:
%=    for(env=E;env;env=cdr(env)) {
%=       lval_e=caar(env);
%=       if(type||cp(e)?car(e)==sym &&(cdr(e)>>4)==type:e==sym) {
%=          if(macro)
%=             PTR_macro=cp(e)&& cdr(e)&8;
%=          return o2c(car(env))+1;
%=       }
%=    }
%=    if(macro)
%=       PTR_macro=(o2a(sym)[8]>>type)&32;
%=    if(type>2) {
%=       dbgr(f,type,sym,&sym);
%=       goto_st;
%=    }
%=    return o2a(sym)+4+type;
%- }
%=    lval_PTR_memory;
%=    lval_PTR_memf;
%=    int_memory_size;
%=    lval_PTR_stack;
%=    lval_xvalues=8;
%=    lval_dyns=0;
%=    jmp_buf_top_jmp;
%=    lval_pkg;
%=    lval_pkgs;
%=    lval_kwp=0;
lisp500_fn([void_gcm(lval_v)]):-true.
%- {
%=    lval_PTR_t;
%=    int_i;
%=  st:   t=(lval_PTR)(v&~3);
%=    if(v &3&& !(t[0] &4)) {
%=       t[0]|=4;
%=       switch(v &3) {
%=       case 1:
%=          gcm(t[0]-4);
%=          v=t[1];
%=          goto_st;
%=       case 2:
%=          gcm(t[1]-4);
%=          if(t[0]>>8) {
%=             for(i=1;i<t[0]>>8;i++)
%=                gcm(t[i+1]);
%=             v=t[i+1];
%=             goto_st;
%=          }
%=       }
%=    }
%- }
lisp500_fn([lval_gc(lval_PTR_f)]):-true.
%- {
%=    int_i;
%=    lval_PTR_m;
%=    int_l;
%=    int_u=0;
%=    int_ml;
%=    printf(";garbage_collectingREST\n");
%=    while(memf) {
%=       lval_PTR_n=(lval_PTR)memf[0];
%=       memset(memf,0,4PTR_memf[1]);
%=       memf=(lval_PTR)n;
%=    }
%=    gcm(xvalues);
%=    gcm(pkgs);
%=    gcm(dyns);
%=    for(;f>stack;f--) {
%=       if((PTRf&3)&&(PTRf<memory||PTRf>memory+memory_size/4))
%=          printf("%x\n",PTR_f);
%=       gcm(PTR_f);
%=    }
%=    memf=0;
%=    m=memory;
%=    i=0;
%=    while(m<memory+memory_size/4) {
%=       l=((m[1] &4?m[0]>>8:0)+1)&~1;
%=       if(m[0] &4) {
%=          if(u) {
%=             m[-ml]=(lval)memf;
%=             m[1-_ml]=ml;
%=             memf=m-ml;
%=             u=0;
%=             i+=ml;
%=          }
%=       } else{
%=          if(!u)
%=             ml=0;
%=          ml+=l+2;
%=          u=1;
%=       }
%=       m[0] &=~4;
%=       m+=l+2;
%=    }
%=    if(u) {
%=       m[-ml]=(lval)memf;
%=       m[1-_ml]=ml;
%=       memf=m-ml;
%=       i+=ml;
%=    }
%=    printf(";done._%d_free.\n",i);
%=    return 0;
%- }
lisp500_fn([lval_PTR_m0(lval_PTR_g,int_n)]):-true.
%- {
%=    lval_PTR_m=memf;
%=    lval_PTR_p=0;
%=    n=(n+1)&~1;
%=    for(;m;m=(lval_PTR)m[0]) {
%=       if(n_<=m[1]) {
%=          if(m[1]==n)
%=             if(p)
%=                p[0]=m[0];
%=             else
%=                memf=(lval_PTR)m[0];
%=          else{
%=             m[1]-=n;
%=             m+=m[1];
%=          }
%=          return m;
%=       } p=m;
%=    } return 0;
%- }
lisp500_fn([lval_PTR_ma0(lval_PTR_g,int_n)]):-true.
%- {
%=    lval_PTR_m;
%=  st:   m=m0(g,n+2);
%=    if(!m) {
%=       gc(g);
%=       goto_st;
%=    } PTR_m=n_<<8;
%=    return m;
%- }
lisp500_fn([lval_PTR_ms0(lval_PTR_g,int_n)]):-true.
%- {
%=    lval_PTR_m;
%=  st:   m=m0(g,(n+12)/4);
%=    if(!m) {
%=       gc(g);
%=       goto_st;
%=    } PTR_m=(n+4)<<6;
%=    return m;
%- }
lisp500_fn([lval_PTR_mb0(lval_PTR_g,int_n)]):-true.
%- {
%=    lval_PTR_m;
%=  st:   m=m0(g,(n+95)/32);
%=    if(!m) {
%=       gc(g);
%=       goto_st;
%=    } PTR_m=(n+31)<<3;
%=    return m;
%- }
lisp500_fn([X_lval_ma(lval_PTR_g,int_n,REST)]):-true.
%- {
%=    va_list_v;
%=    int_i;
%=    lval_PTR_m;
%=  st:   va_start(v,n);
%=    m=m0(g,n+2);
%=    if(!m) {
%=       for(i=-1;i<n;i++)
%=          gcm(va_arg(v,lval));
%=       gc(g);
%=       goto_st;
%=    }
%=    PTR_m=n_<<8;
%=    for(i=-1;i<n;i++)
%=       m[2+i]=va_arg(v,lval);
%=    return a2o(m);
%- }
lisp500_fn([X_lval_ms(lval_PTR_g,int_n,REST)]):-true.
%- {
%=    va_list_v;
%=    int_i;
%=    lval_PTR_m;
%=  st:   va_start(v,n);
%=    m=m0(g,n+2);
%=    if(!m) {
%=       gc(g);
%=       goto_st;
%=    } PTR_m=n_<<8;
%=    for(i=-1;i<n;i++)
%=       m[2+i]=va_arg(v,lval);
%=    return s2o(m);
%- }
lisp500_fn([double_o2d(lval_o)]):-true.
%- {
%=    return sp(o)?PTR(double_PTR)(o2s(o)+2):o>>5;
%- }
lisp500_fn([lval_d2o(lval_PTR_g,double_d)]):-true.
%- {
%=    lval_x=(lval)d<<5|16;
%=    lval_PTR_a;
%=    if(o2d(x)==d)
%=       return x;
%=    a=ma0(g,2);
%=    a[1]=84;
%=    PTR(double_PTR)(a+2)=d;
%=    return s2o(a);
%- }
lisp500_fn([int_o2i(lval_o)]):-true.
%- {
%=    return (int)o2d(o);
%- }
lisp500_fn([unsigned_o2u(lval_o)]):-true.
%- {
%=    return (unsigned)o2d(o);
%- }
lisp500_fn([lval_cons(lval_PTR_g,lval_a,lval_d)]):-true.
%- {
%=    lval_PTR_c=m0(g,2);
%=    if(!c) {
%=       gcm(a);
%=       gcm(d);
%=       gc(g);
%=       c=m0(g,2);
%=    } c[0]=a;
%=    c[1]=d;
%=    return c2o(c);
%- }
lisp500_fn([int_string_equal_do(lval_a,lval_b)]):-true.
%- {
%=    int_i;
%=    for(i=0;i<o2s(a)[0]/64-4;i++)
%=       if(o2z(a)[i]_!=o2z(b)[i])
%=          return 0;
%=    return 1;
%- }
lisp500_fn([int_string_equal(lval_a,lval_b)]):-true.
%- {
%=    return a==b||(sp(a)&& sp(b)&&
%=       o2s(a)[1]==20&& o2s(b)[1]==20&& o2s(a)[0]==o2s(b)[0]
%=           && string_equal_do(a,b));
%- }
lisp500_fn([lval_argi(lval_a,lval_PTR_b)]):-true.
%- {
%=    if(cp(a)) {
%=       PTR_b=cdr(a);
%=       return car(a);
%=    }
%=    PTR_b=0;
%=    return a;
%- }
lisp500_fn([lval_rest(lval_PTR_h,lval_PTR_g)]):-true.
%- {
%=    lval_PTR_f=h-1;
%=    lval_r=0;
%=    for(;f>=g;f--)
%=       r=cons(h,PTR_f,r);
%=    return r;
%- }
lisp500_fn([lval_args(lval_PTR,lval,int)]):-true.
%- {
%- }
lisp500_fn([lval_argd(lval_PTR_f,lval_n,lval_a)]):-true.
%- {
%=    if(cp(n)) {
%=       lval_PTR_h=f;
%=       for(;a;a=cdr(a))
%=          PTR++h=car(a);
%=       ++h;
%=       PTR++h=PTR_f;
%=       return args(f,n,h-f-2);
%=    }
%=    return cons(f,cons(f,n,a),PTR_f);
%- }
lisp500_fn([lval_args(lval_PTR_f,lval_m,int_c)]):-true.
%- {
%=    lval_PTR_g=f+1;
%=    lval_PTR_h=f+c+2;
%=    int_t;
%=    lval_k,PTR_l;
%=  st:   t=0;
%=    while(cp(m)) {
%=       lval_n=car(m);
%=       m=cdr(m);
%=       switch(cp(n)?-1:o2a(n)[7]>>3) {
%=       case 2:
%=       case 3:
%=          t=1;
%=          continue;
%=       case 4:
%=          t=2;
%=          continue;
%=       case 5:
%=          t=-2;
%=          continue;
%=       case 6:
%=          t=4;
%=          continue;
%=       case 7:
%=          t=5;
%=          continue;
%=       default:
%=          switch(t) {
%=          case 0:
%=             if(g>=h-1) {
%=                dbgr(g,7,0,h);
%=                goto_st;
%=             } PTR_h=argd(h,n,PTR_g);
%=             break;
%=          case 1:
%=             PTR_h=cons(h,cons(h,n,rest(h-1,g)),PTR_h);
%=             t=-1;
%=             continue;
%=          case 2:
%=             n=argi(n,&k);
%=             PTR_h=argd(h,n,g<h-1?PTRg:evca(h,k));
%=             break;
%=          case -2:
%=             n=argi(n,&k);
%=             for(l=g;l<h-1;l+=2)
%=                if(string_equal(o2a(n)[2],o2a(PTR_l)[2])&& o2a(PTR_l)[9]==kwp) {
%=                   k=l[1];
%=                   break;
%=                } PTR_h=argd(h,n,l<h-1?k:evca(h,k));
%=             continue;
%=          case 4:
%=             PTR_h=cons(h,cons(h,n,rest(h-1,f+1)),PTR_h);
%=             t=0;
%=             continue;
%=          case 5:
%=             PTR_h=cons(h,cons(h,n,f[-1]),PTR_h);
%=             t=0;
%=             continue;
%=          }
%=       } g++;
%=    } if(m)
%=       return cons(h,cons(h,m,rest(h-1,g)),PTR_h);
%=    if(g<h-1&&t>=0) {
%=       h[-1]=(c_<<5)|16;
%=       dbgr(h,6,0,h);
%=       goto_st;
%=    } return PTR_h;
%- }
lisp500_fn([lval_eval_body(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(1)T=0;
%=    for(;ex;ex=cdr(ex))
%=       T=evca(g,ex);
%=    return T;
%- }
lisp500_fn([int_map_eval(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_PTR_g=f+3;
%=    for(;ex;ex=cdr(ex),g++) {
%=       g[-1]=((g-f-3)<<5)|16;
%=       PTR_g=PTR_f;
%=       g[-1]=evca(g,ex);
%=    } return g-f-3;
%- }
lisp500_fn([lval_eval(lval_PTR_f,lval_expr)]):-true.
%- {
%=    NF(1)T=0;
%=    T=cons(g,expr,0);
%=    return evca(g,T);
%- }
lisp500_fn([lval_rvalues(lval_PTR_g,lval_v)]):-true.
%- {
%=    return xvalues==8?cons(g,v,0):xvalues;
%- }
lisp500_fn([lval_mvalues(lval_a)]):-true.
%- {
%=    xvalues=a;
%=    return car(a);
%- }
lisp500_fn([lval_infn(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    jmp_buf_jmp;
%=    lval_vs;
%=    lval_PTR_g=h+1;
%=    lval_fn=PTR_f;
%=    int_d=h-f-1;
%=    h[1]=o2a(fn)[3];
%=    NE=args(f,o2a(fn)[4],d);
%=    g[-1]=cons(g,dyns,ms(g,1,20,&jmp));
%=    NE=cons(g,cons(g,cons(g,o2a(fn)[6],64),g[-1]),NE);
%=    g[-1]=(d_<<5)|16;
%=    if(!(vs=setjmp(jmp)))
%=       return eval_body(g,o2a(fn)[5]);
%=    return mvalues(car(vs));
%- }
lisp500_fn([X_lval_call(lval_PTR_f,lval_fn,unsigned_d)]):-true.
%- {
%=    lval_PTR_g=f+d+3;
%=    xvalues=8;
%=    if(o2a(fn)[1]==20)
%=       fn=o2a(fn)[5];
%=    if(o2a(fn)[0] &16)
%=       fn=o2a(fn)[3];
%=    PTR++f=fn;
%=    fn=o2a(fn)[2];
%=    if(d_<(unsigned)o2s(fn)[3])
%=       dbgr(g,7,0,f);
%=    if(d>(unsigned)o2s(fn)[4])
%=       dbgr(g,6,0,f);
%=    return ((lval(PTR)())o2s(fn)[2])(f,f+d+1);
%- }
lisp500_fn([lval_eval_quote(lval_PTR_g,lval_ex)]):-true.
%- {
%=    return car(ex);
%- }
lisp500_fn([int_specp(lval_PTR_f,lval_ex,lval_s)]):-true.
%- {
%=    for(;ex;ex=cdr(ex))
%=       if(ap(caar(ex))&& o2a(caar(ex))[7]==3<<3) {
%=          lval_e=cdar(ex);
%=          for(;e;e=cdr(e))
%=             if(o2a(caar(e))[7]==4<<3) {
%=                lval_sp=cdar(e);
%=                for(;sp;sp=cdr(sp))
%=                   if(car(sp)==s)
%=                      return 1;
%=             }
%=       } else
%=          break;
%=    return 0;
%- }
lisp500_fn([void_unwind(lval_PTR_f,lval_c)]):-true.
%- {
%=    lval_e;
%=    NF(0)for(;dyns_!=c;dyns=cdr(dyns))
%=       if(ap(car(dyns)))
%=          if(o2a(car(dyns))[1]==52) {
%=             NE=o2a(car(dyns))[2];
%=             eval_body(g,o2a(car(dyns))[3]);
%=          } else
%=             for(e=o2a(car(dyns))[2];e;e=cdr(e))
%=                o2a(caar(e))[4]=cdar(e);
%=       else
%=          o2s(car(dyns))[2]=0;
%- }
lisp500_fn([lval_eval_let(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_r;
%=    NF(3)T=car(ex);
%=    U=E;
%=    V=0;
%=    r=ma(g,1,84,0);
%=    dyns=cons(g,r,dyns);
%=    for(;T;T=cdr(T)) {
%=       V=evca(g,cdar(T));
%=       if(o2a(caar(T))[8] &128||specp(g,cdr(ex),caar(T))) {
%=          o2a(r)[2]=cons(g,cons(g,caar(T),V),o2a(r)[2]);
%=       } else
%=          U=cons(g,cons(g,caar(T),V),U);
%=    } for(r=o2a(r)[2];r;r=cdr(r)) {
%=       T=o2a(caar(r))[4];
%=       o2a(caar(r))[4]=cdar(r);
%=       set_cdr(car(r),T);
%=       U=cons(g,cons(g,caar(r),-8),U);
%=    } NE=U;
%=    T=eval_body(g,cdr(ex));
%=    unwind(g,cdr(dyns));
%=    return T;
%- }
lisp500_fn([lval_eval_letm(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_r;
%=    NF(2)T=U=0;
%=    r=ma(g,1,84,0);
%=    dyns=cons(g,r,dyns);
%=    for(T=car(ex);T;T=cdr(T)) {
%=       U=evca(g,cdar(T));
%=       if(o2a(caar(T))[8] &128||specp(g,cdr(ex),caar(T))) {
%=          o2a(r)[2]=cons(g,cons(g,caar(T),o2a(caar(T))[4]),o2a(r)[2]);
%=          o2a(caar(T))[4]=U;
%=          U=-8;
%=       } U=cons(g,caar(T),U);
%=       NE=cons(g,U,NE);
%=    }
%=    T=eval_body(g,cdr(ex));
%=    unwind(g,cdr(dyns));
%=    return T;
%- }
lisp500_fn([lval_eval_progv(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_r;
%=    NF(2)T=U=0;
%=    r=ma(g,1,84,0);
%=    T=evca(g,ex);
%=    U=evca(g,cdr(ex));
%=    dyns=cons(g,r,dyns);
%=    for(;T&& U;T=cdr(T),U=cdr(U)) {
%=       o2a(r)[2]=cons(g,cons(g,car(T),o2a(car(T))[4]),o2a(r)[2]);
%=       o2a(car(T))[4]=car(U);
%=    } T=eval_body(g,cddr(ex));
%=    unwind(f,cdr(dyns));
%=    return T;
%- }
lisp500_fn([lval_eval_flet(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(4)V=W=0;
%=    U=E;
%=    for(T=car(ex);T;T=cdr(T)) {
%=       V=ma(g,5,212,ms(f,3,212,infn,0,-1),E,cadr(car(T)),cddr(car(T)),caar(T));
%=       W=cons(g,caar(T),16);
%=       V=cons(g,W,V);
%=       U=cons(g,V,U);
%=    } NE=U;
%=    return eval_body(g,cdr(ex));
%- }
lisp500_fn([lval_eval_labels(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(4)V=W=0;
%=    U=E;
%=    for(T=car(ex);T;T=cdr(T))
%=       U=cons(g,0,U);
%=    NE=U;
%=    for(T=car(ex);T;T=cdr(T),U=cdr(U)) {
%=       V=ma(g,5,212,ms(f,3,212,infn,0,-1),NE,cadr(car(T)),cddr(car(T)),caar(T));
%=       W=cons(g,caar(T),16);
%=       set_car(U,cons(g,W,V));
%=    } return eval_body(g,cdr(ex));
%- }
lisp500_fn([lval_eval_macrolet(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(4)V=W=0;
%=    U=E;
%=    for(T=car(ex);T;T=cdr(T)) {
%=       V=ma(g,5,212,ms(f,3,212,infn,0,-1),E,cadr(car(T)),cddr(car(T)),caar(T));
%=       W=cons(g,caar(T),24);
%=       V=cons(g,W,V);
%=       U=cons(g,V,U);
%=    } NE=U;
%=    return eval_body(g,cdr(ex));
%- }
lisp500_fn([lval_eval_symbol_macrolet(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(3)V=0;
%=    U=E;
%=    for(T=car(ex);T;T=cdr(T)) {
%=       V=cons(g,caar(T),8);
%=       V=cons(g,V,cadr(car(T)));
%=       U=cons(g,V,U);
%=    } NE=U;
%=    return eval_body(g,cdr(ex));
%- }
lisp500_fn([lval_eval_setq(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_r;
%=    do{
%=       r=evca(f,cdr(ex));
%=       PTR_binding(f,car(ex),0,0)=r;
%=       ex=cddr(ex);
%=    } while(ex);
%=    return r;
%- }
lisp500_fn([lval_eval_function(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_x;
%=    ex=car(ex);
%=    if(cp(ex))
%=       if(car(ex)==symi[75].sym) {
%=          lval_n=0;
%=          x=cddr(ex);
%=          if(!cdr(x)&& caar(x)==symi[23].sym) {
%=             x=car(x);
%=             n=cadr(x);
%=             x=cddr(x);
%=          }
%=          return ma(f,5,212,ms(f,3,212,infn,0,-1),E,cadr(ex),x,n);
%=       } else
%=          x=PTR_binding(f,cadr(ex),2,0);
%=    else
%=       x=PTR_binding(f,ex,1,0);
%=    if(x_!=8)
%=       return x;
%=    dbgr(f,1,ex,&x);
%=    return x;
%- }
lisp500_fn([lval_eval_tagbody(lval_PTR_f,lval_ex)]):-true.
%- {
%=    jmp_buf_jmp;
%=    lval_tag;
%=    lval_e;
%=    NF(2)T=U=0;
%=    U=ms(g,1,52,&jmp);
%=    dyns=cons(g,U,dyns);
%=    for(e=ex;e;e=cdr(e))
%=       if(ap(car(e))) {
%=          T=cons(g,dyns,U);
%=          NE=cons(g,cons(g,cons(g,car(e),48),T),NE);
%=       } e=ex;
%=  again:
%=    if(!(tag=setjmp(jmp))) {
%=       for(;e;e=cdr(e))
%=          if(!ap(car(e)))
%=             evca(g,e);
%=    } else
%=       for(e=ex;e;e=cdr(e))
%=          if(car(e)==tag) {
%=             e=cdr(e);
%=             goto_again;
%=          }
%=    unwind(g,cdr(dyns));
%=    return 0;
%- }
lisp500_fn([lval_eval_go(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_b=PTR_binding(f,car(ex),3,0);
%=    if(o2s(cdr(b))[2]) {
%=       unwind(f,car(b));
%=       longjmp(PTR(jmp_buf_PTR)(o2s(cdr(b))[2]),car(ex));
%=    } dbgr(f,9,car(ex),&ex);
%=    longjmp(top_jmp,1);
%- }
lisp500_fn([lval_eval_block(lval_PTR_f,lval_ex)]):-true.
%- {
%=    jmp_buf_jmp;
%=    lval_vs;
%=    NF(2)T=U=0;
%=    T=ms(g,1,52,&jmp);
%=    U=cons(g,dyns,T);
%=    dyns=cons(g,T,dyns);
%=    NE=cons(g,cons(g,cons(g,car(ex),64),U),NE);
%=    if(!(vs=setjmp(jmp))) {
%=       T=eval_body(g,cdr(ex));
%=       unwind(g,cdr(dyns));
%=       return T;
%=    }
%=    return mvalues(car(vs));
%- }
lisp500_fn([lval_eval_return from(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_b;
%=    jmp_buf_PTR_jmp;
%=    NF(1)T=0;
%=    b=PTR_binding(g,car(ex),4,0);
%=    jmp=(jmp_buf_PTR)o2s(cdr(b))[2];
%=    if(jmp) {
%=       unwind(g,car(b));
%=       T=rvalues(g,evca(g,cdr(ex)));
%=       longjmp(PTR_jmp,cons(g,T,0));
%=    } dbgr(g,8,car(ex),&T);
%=    longjmp(top_jmp,1);
%- }
lisp500_fn([lval_eval_catch(lval_PTR_f,lval_ex)]):-true.
%- {
%=    jmp_buf_jmp;
%=    lval_vs;
%=    lval_oc=dyns;
%=    NF(2)
%=       T=U=0;
%=    U=evca(g,ex);
%=    T=ms(g,1,20,&jmp);
%=    T=cons(g,U,T);
%=    dyns=cons(g,T,dyns);
%=    if(!(vs=setjmp(jmp)))
%=       vs=eval_body(g,cdr(ex));
%=    else
%=       vs=mvalues(car(vs));
%=    dyns=oc;
%=    return vs;
%- }
lisp500_fn([lval_eval_throw(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_c;
%=    NF(1)T=0;
%=    T=evca(g,ex);
%=  st:
%=    for(c=dyns;c;c=cdr(c))
%=       if(cp(car(c))&& caar(c)==T) {
%=          unwind(g,c);
%=          T=evca(g,cdr(ex));
%=          T=rvalues(g,T);
%=          longjmp(PTR(jmp_buf_PTR)(o2s(cdar(c))[2]),cons(g,T,0));
%=       }
%=    dbgr(g,5,T,&T);
%=    goto_st;
%- }
lisp500_fn([lval_eval_unwind_protect(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(1)T=0;
%=    T=ma(g,2,52,E,cdr(ex));
%=    dyns=cons(g,T,dyns);
%=    T=evca(g,ex);
%=    T=rvalues(g,T);
%=    unwind(g,cdr(dyns));
%=    return mvalues(T);
%- }
lisp500_fn([lval_eval_if(lval_PTR_f,lval_ex)]):-true.
%- {
%=    return evca(f,evca(f,ex)?cdr(ex):cddr(ex));
%- }
lisp500_fn([lval_eval_multiple_value_call(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_PTR_g=f+3;
%=    lval_l;
%=    f[1]=evca(f,ex);
%=    for(ex=cdr(ex);ex;ex=cdr(ex)) {
%=       PTR_g=PTR_f;
%=       g[-1]=((g-f-3)<<5)|16;
%=       for(l=rvalues(g,evca(g,ex));l;l=cdr(l)) {
%=          g[-1]=car(l);
%=          g++;
%=       }
%=    } xvalues=8;
%=    return call(f,f[1],g-f-3);
%- }
lisp500_fn([lval_eval_multiple_value_prog1(lval_PTR_f,lval_ex)]):-true.
%- {
%=    NF(1)T=0;
%=    T=evca(g,ex);
%=    T=rvalues(g,T);
%=    eval_body(g,cdr(ex));
%=    return mvalues(T);
%- }
lisp500_fn([lval_eval_declare(lval_PTR_f,lval_ex)]):-true.
%- {
%=    return 0;
%- }
lisp500_fn([lval_l2(lval_PTR_f,lval_a,lval_b)]):-true.
%- {
%=    return cons(f,a,cons(f,b,0));
%- }
lisp500_fn([lval_eval_setf(lval_PTR_f,lval_ex)]):-true.
%- {
%=    lval_r;
%=    int_m;
%=    NF(1)T=0;
%=  ag:   if(!cp(car(ex))) {
%=       r=PTR_binding(g,car(ex),0,&m);
%=       if(!m)
%=          return PTR_binding(g,car(ex),0,0)
%=             =evca(g,cdr(ex));
%=       set_car(ex,r);
%=       goto_ag;
%=    } r=PTR_binding(g,caar(ex),2,0);
%=    if(r==8)
%=       dbgr(g,1,l2(f,symi[33].sym,caar(ex)),&r);
%=    T=cons(g,cadr(ex),cdar(ex));
%=    return call(g,r,map_eval(g,T));
%- }
lisp500_fn([lval_llist(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    return rest(h,f+1);
%- }
lisp500_fn([lval_lvalues(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    return mvalues(rest(h,f+1));
%- }
lisp500_fn([lval_lfuncall(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    return call(f,f[1],h-f-2);
%- }
lisp500_fn([lval_lapply(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    while(h[-1]) {
%=       h[0]=cdr(h[-1]);
%=       h[-1]=car(h[-1]);
%=       h++;
%=    } return call(f,f[1],h-f-3);
%- }
lisp500_fn([lval_leq(lval_PTR_f)]):-true.
%- {
%=    return f[1]==f[2]?TRUE_:0;
%- }
lisp500_fn([lval_lcons(lval_PTR_f)]):-true.
%- {
%=    return cons(f,f[1],f[2]);
%- }
lisp500_fn([lval_lcar(lval_PTR_f)]):-true.
%- {
%=    return car(f[1]);
%- }
lisp500_fn([lval_setfcar(lval_PTR_f)]):-true.
%- {
%=    return set_car(f[2],f[1]);
%- }
lisp500_fn([lval_lcdr(lval_PTR_f)]):-true.
%- {
%=    return cdr(f[1]);
%- }
lisp500_fn([lval_setfcdr(lval_PTR_f)]):-true.
%- {
%=    return set_cdr(f[2],f[1]);
%- }
lisp500_fn([lval_lequ(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_s=o2d(f[1]);
%=    for(f+=2;f<h;f++)
%=       if(s_!=o2d(PTR_f))
%=          return 0;
%=    return TRUE;
%- }
lisp500_fn([lval_lless(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_s=o2d(f[1]);
%=    for(f+=2;f<h;f++)
%=       if(s<o2d(PTR_f))
%=          s=o2d(PTR_f);
%=       else
%=          return 0;
%=    return TRUE;
%- }
lisp500_fn([lval_lplus(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_s=0;
%=    for(f++;f<h;f++)
%=       s+=o2d(PTR_f);
%=    return d2o(f,s);
%- }
lisp500_fn([lval_lminus(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_s=o2d(f[1]);
%=    f+=2;
%=    if(f<h)
%=       for(;f<h;f++)
%=          s-=o2d(PTR_f);
%=    else
%=       s=-s;
%=    return d2o(f,s);
%- }
lisp500_fn([lval_ltimes(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_s=1;
%=    for(f++;f<h;f++)
%=       s_PTR=o2d(PTR_f);
%=    return d2o(f,s);
%- }
lisp500_fn([lval_ldivi(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_s=o2d(f[1]);
%=    f+=2;
%=    if(f<h)
%=       for(;f<h;f++)
%=          s/=o2d(PTR_f);
%=    else
%=       s=1/_s;
%=    return d2o(f,s);
%- }
lisp500_fn([lval_ldpb(lval_PTR_f)]):-true.
%- {
%=    int_s=o2i(car(f[2]));
%=    int_p=o2i(cdr(f[2]));
%=    int_m=(1<<s)-1;
%=    return d2o(f,(o2i(f[1])&m)<<p|(o2i(f[3])&~(m_<<p)));
%- }
lisp500_fn([lval_lldb(lval_PTR_f)]):-true.
%- {
%=    int_s=o2i(car(f[1]));
%=    int_p=o2i(cdr(f[1]));
%=    return d2o(f,o2i(f[2])>>p&((1<<s)-1));
%- }
lisp500_fn([lval_lfloor(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    double_n=o2d(f[1]);
%=    double_d=h-f>2?o2d(f[2]):1;
%=    double_q=floor(n/d);
%=    return mvalues(l2(f,d2o(f,q),d2o(f,n-q_PTR_d)));
%- }
% #int_gensymc=0;
lisp500_fn([lval_lgensym(lval_PTR_f)]):-true.
%- {
%=    lval_PTR_r=ms0(f,4);
%=    r[1]=20;
%=    sprintf((char_PTR)(r+2),
%=       "g%3.3d",gensymc++);
%=    return ma(f,9,20,s2o(r),0,8,8,8,-8,16,0,0);
%- }
lisp500_fn([lval_lcode_char(lval_PTR_f)]):-true.
%- {
%=    unsigned_int c=o2u(f[1]);
%=    return c<256?32PTR_c+24:0;
%- }
lisp500_fn([lval_lchar_code(lval_PTR_f)]):-true.
%- {
%=    return f[1]&~8;
%- }
lisp500_fn([lval_lmakef(lval_PTR_f)]):-true.
%- {
%=    return d2o(f,f-stack);
%- }
lisp500_fn([lval_lfref(lval_PTR_f)]):-true.
%- {
%=    return stack[o2i(f[1])];
%- }
lisp500_fn([lval_stringify(lval_PTR_f,lval_l)]):-true.
%- {
%=    int_i;
%=    lval_PTR_r;
%=    lval_t=l;
%=    PTR++f=l;
%=    for(i=0;t;i++,t=cdr(t));
%=    r=ms0(f,i);
%=    r[1]=20;
%=    ((char_PTR)r)[i+8]=0;
%=    for(i=8;l;i++,l=cdr(l))
%=       ((char_PTR)r)[i]=car(l)>>5;
%=    return s2o(r);
%- }
lisp500_fn([lval_lstring(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    return stringify(f,rest(h,f+1));
%- }
lisp500_fn([lval_lival(lval_PTR_f)]):-true.
%- {
%=    return d2o(f,f[1]);
%- }
lisp500_fn([lval_lmakei(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    int_i=2;
%=    int_l=o2i(f[1]);
%=    lval_PTR_r=ma0(h,l);
%=    r[1]=f[2]|4;
%=    memset(r+2,0,4PTR_o2i(f[1]));
%=    for(f+=3;f<h;f++,i++) {
%=       if(i>=l+2)
%=          printf("overinitializing_in_makei\n");
%=       r[i]=PTR_f;
%=    }
%=    return a2o(r);
%- }
lisp500_fn([lval_liboundp(lval_PTR_f)]):-true.
%- {
%=    return o2a(f[1])[o2u(f[2])]==8?0:TRUE;
%- }
lisp500_fn([lval_limakunbound(lval_PTR_f)]):-true.
%- {
%=    o2a(f[1])[o2u(f[2])]=8;
%=    return 0;
%- }
lisp500_fn([lval_liref(lval_PTR_f)]):-true.
%- {
%=    if(o2u(f[2])>=o2a(f[1])[0]/256+2)
%=       write(1,"out_of_bounds_in_iref\n",22);
%=    return ((lval_PTR)(f[1]&~3))[o2u(f[2])]&~4;
%- }
lisp500_fn([lval_setfiref(lval_PTR_f)]):-true.
%- {
%=    int_i=o2i(f[3]);
%=    if(i>=o2a(f[2])[0]/256+2)
%=       printf("out_of_bounds_in_setf_iref\n");
%=    return ((lval_PTR)(f[2]&~3))[i]=i==1?f[1]|4:f[1];
%- }
lisp500_fn([lval_lmakej(lval_PTR_f)]):-true.
%- {
%=    lval_PTR_r=mb0(f,o2i(f[1]));
%=    r[1]=o2i(f[2]);
%=    memset(r+2,0,(o2i(f[1])+7)/8);
%=    return s2o(r);
%- }
lisp500_fn([lval_ljref(lval_PTR_f)]):-true.
%- {
%=    return d2o(f,o2s(f[1])[o2u(f[2])]);
%- }
lisp500_fn([lval_setfjref(lval_PTR_f)]):-true.
%- {
%=    return o2s(f[2])[o2u(f[3])]=o2u(f[1]);
%- }
% #ifdef_WIN32
lisp500_fn([lval_lmake_fs(lval_PTR_f)]):-true.
%- {
%=    HANDLE_fd=CreateFile(o2z(f[1]),f[2]?GENERIC_WRITE:
%=          GENERIC_READ,f[2]?FILE_SHARE_WRITE:FILE_SHARE_READ,NULL,OPEN_EXISTING,
%=          FILE_ATTRIBUTE_NORMAL,NULL);
%=    return ms(f,4,116,1,fd,f[2],0);
%- }
lisp500_fn([lval_lclose_fs(lval_PTR_f)]):-true.
%- {
%=    CloseHandle(o2s(f[1])[3]);
%=    return 0;
%- }
lisp500_fn([lval_llisten_fs(lval_PTR_f)]):-true.
%- {
%=    return WaitForSingleObject(o2s(f[1])[3],0)==WAIT_OBJECT0?TRUE_:0;
%- }
lisp500_fn([lval_lread_fs(lval_PTR_f)]):-true.
%- {
%=    int_l=o2i(f[3]);
%=    if(!ReadFile(o2s(f[1])[3],
%=       o2z(f[2])+l,(o2s(f[2])[0]>>6)-4-_l,&l,NULL))
%=       return 0;
%=    return d2o(f,l);
%- }
lisp500_fn([lval_lwrite_fs(lval_PTR_f)]):-true.
%- {
%=    int_l=o2i(f[3]);
%=    if(!WriteFile(o2s(f[1])[3],
%=       o2z(f[2])+l,o2i(f[4])-l,&l,NULL))
%=       return 0;
%=    return d2o(f,l);
%- }
lisp500_fn([lval_lfinish_fs(lval_PTR_f)]):-true.
%- {
%=    FlushFileBuffers(o2s(f[1])[3]);
%=    return 0;
%- }
lisp500_fn([lval_lfasl(lval_PTR_f)]):-true.
%- {
%=    HMODULE_h;
%=    FARPROC_s;
%=    h=LoadLibrary(o2z(f[1]));
%=    s=GetProcAddress(h,"init");
%=    return s(f);
%- }
lisp500_fn([lval_luname(lval_PTR_f)]):-true.
%- {
%=    OSVERSIONINFO_osvi;
%=    osvi.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
%=    GetVersionEx(&osvi);
%=    f[1]=cons(f+1,strf(f+1,osvi.szCSDVersion),0);
%=    f[1]=cons(f+1,d2o(f+1,osvi.dwBuildNumber),f[1]);
%=    f[1]=cons(f+1,d2o(f+1,osvi.dwMinorVersion),f[1]);
%=    return cons(f+1,d2o(f,osvi.dwMajorVersion),f[1]);
%- }
% #else
lisp500_fn([lval_lmake_fs(lval_PTR_f)]):-true.
%- {
%=    int_fd=open(o2z(f[1]),f[2]?O_WRONLY|O_CREAT|O_TRUNC:O_RDONLY,0600);
%=    return fd>=0?ms(f,4,116,1,fd,f[2],0):d2o(f,errno);
%- }
lisp500_fn([lval_lclose_fs(lval_PTR_f)]):-true.
%- {
%=    close(o2s(f[1])[3]);
%=    return 0;
%- }
lisp500_fn([lval_llisten_fs(lval_PTR_f)]):-true.
%- {
%=    fd_set_r;
%=    struct_timeval_t;
%=    t.tv_sec=0;
%=    t.tv_usec=0;
%=    FD_ZERO(&r);
%=    FD_SET(o2s(f[1])[3],&r);
%=    return select(o2s(f[1])[3]+1,&r,NULL,NULL,&t)?TRUE_:0;
%- }
lisp500_fn([lval_lread_fs(lval_PTR_f)]):-true.
%- {
%=    int_l=o2i(f[3]);
%=    l=read(o2s(f[1])[3],o2z(f[2])+l,
%=       (o2s(f[2])[0]>>6)-4-_l);
%=    return l<0?cons(f,errno,0):d2o(f,l);
%- }
lisp500_fn([lval_lwrite_fs(lval_PTR_f)]):-true.
%- {
%=    int_l=o2i(f[3]);
%=    l=write(o2s(f[1])[3],
%=       o2z(f[2])+l,o2i(f[4])-l);
%=    return l<0?cons(f,errno,0):d2o(f,l);
%- }
lisp500_fn([lval_lfinish_fs(lval_PTR_f)]):-true.
%- {
%=    fsync(o2s(f[1])[3]);
%=    return 0;
%- }
lisp500_fn([lval_lfasl(lval_PTR_f)]):-true.
%- {
%=    void_PTR_h;
%=    lval(PTR_s)();
%=    h=dlopen(o2z(f[1]),RTLD_NOW);
%=    s=dlsym(h,"init");
%=    return s(f);
%- }
lisp500_fn([lval_luname(lval_PTR_f)]):-true.
%- {
%=    struct_utsname_un;
%=    uname(&un);
%=    f[1]=cons(f+1,strf(f+1,un.machine),0);
%=    f[1]=cons(f+1,strf(f+1,un.version),f[1]);
%=    f[1]=cons(f+1,strf(f+1,un.release),f[1]);
%=    return cons(f+1,strf(f,un.sysname),f[1]);
%- }
% #endif
% #FILE_PTR_ins;
lisp500_fn([void_load(lval_PTR_f,char_PTR_s)]):-true.
%- {
%=    lval_r;
%=    FILE_PTR_oldins=ins;
%=    ins=fopen(s,"r");
%=    if(ins) {
%=       do
%=          r=eval(f,lread(f));
%=       while(r_!=8);
%=       fclose(ins);
%=    }
%=    ins=oldins;
%- }
lisp500_fn([lval_lload(lval_PTR_f)]):-true.
%- {
%=    load(f,o2z(f[1]));
%=    return symi[1].sym;
%- }
lisp500_fn([lval_lstring_equal(lval_PTR_f)]):-true.
%- {
%=    return string_equal(f[1],f[2])?TRUE_:0;
%- }
lisp500_fn([lval_leval(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    f[-1]=h-f>2?f[2]_:0;
%=    return eval(f-1,f[1]);
%- }
lisp500_fn([void_psym(lval_p,lval_n)]):-true.
%- {
%=    int_i;
%=    if(!p)
%=       printf("#:");
%=    else_if(p_!=pkg) {
%=       lval_m=car(o2a(p)[2]);
%=       for(i=0;i<o2s(m)[0]/64-4;i++)
%=          putchar(o2z(m)[i]);
%=       putchar(':');
%=    } for(i=0;i<o2s(n)[0]/64-4;i++)
%=       putchar(o2z(n)[i]);
%- }
lisp500_fn([void_print(lval_x)]):-true.
%- {
%=    int_i;
%=    switch(x &3) {
%=    case 0:
%=       if(x)
%=          if(x &8)
%=             if(x>>5<256&& isgraph(x>>5))
%=                printf("#\\%c",x>>5);
%=             else
%=                printf("#\\U+%d",x>>5);
%=          else
%=             printf("%d",x>>5);
%=       else
%=          printf("nil");
%=       break;
%=    case 1:
%=       printf("(");
%=       print(car(x));
%=       for(x=cdr(x);cp(x);x=cdr(x)) {
%=          printf("_");
%=          print(car(x));
%=       }
%=       if(x) {
%=          printf(".");
%=          print(x);
%=       } printf(")");
%=       break;
%=    case 2:
%=       switch(o2a(x)[1]) {
%=       case 212:
%=          printf("#<function_");
%=          print(o2a(x)[6]);
%=          printf(">");
%=          break;
%=       case 20:
%=          psym(o2a(x)[9],o2a(x)[2]);
%=          break;
%=       case 116:
%=          printf("#(");
%=          for(i=0;i<o2a(x)[0]>>8;i++) {
%=             if(i)
%=                printf("_");
%=             print(o2a(x)[i+2]);
%=          } printf(")");
%=          break;
%=       case 180:
%=          printf("#<package_");
%=          print(car(o2a(x)[2]));
%=          printf(">");
%=          break;
%=       default:
%=          if(ap(o2a(x)[1])) {
%=             printf("#<");
%=             print(o2a(o2a(o2a(x)[1]-4)[2])[2]);
%=             printf(">");
%=          } else{
%=             printf("#(");
%=             for(i=0;i<=o2a(x)[0]>>8;i++)
%=                print(o2a(x)[i+1]);
%=             printf(")");
%=          }
%=       } break;
%=    case 3:
%=       switch(o2s(x)[1]) {
%=       case 20:
%=          printf("\"");
%=          for(i=0;i<o2s(x)[0]/64-4;i++) {
%=             char_c=o2z(x)[i];
%=             printf((c=='\\'||c=='\"'?"\\%c":"%c"),c);
%=          } printf("\"");
%=          break;
%=       case 84:
%=          printf("%g",o2d(x));
%=       }
%=    }
%- }
lisp500_fn([lval_lprint(lval_PTR_f)]):-true.
%- {
%=    print(f[1]);
%=    return f[1];
%- }
lisp500_fn([int_ep(lval_PTR_g,lval_expr)]):-true.
%- {
%=    int_i;
%=    lval_v=rvalues(g,eval(g,expr));
%=    if(car(v)==8)
%=       return 0;
%=    if(v)
%=       for(i=0;v;v=cdr(v)) {
%=          printf(";%d:",i++);
%=          print(car(v));
%=          printf("\n");
%=       }
%=    else
%=       printf(";no_values\n");
%=    return 1;
%- }
lisp500_array([char_PTR_exmsg_ARRAY,["variable_unbound","function_unbound","array_index_out_of_bounds","go_tag_not_bound","block_name_not_bound","catch_tag_not_dynamically_bound","too_many_arguments","too_few_arguments","dynamic_extent_of_block_exited","dynamic_extent_of_tagbody_exited"]);
lisp500_fn([int_dbgr(lval_PTR_f,int_x,lval_val,lval_PTR_vp)]):-true.
%- {
%=    lval_ex;
%=    int_i;
%=    lval_PTR_h=f;
%=    int_l=0;
%=    NF(0)ex=o2a(symi[59].sym)[5];
%=    if(ex_!=8) {
%=       h++;
%=       PTR++h=d2o(f,x);
%=       PTR++h=val;
%=       ex=call(f,ex,h-f-1);
%=       longjmp(top_jmp,1);
%=    }
%=    printf(";exception:%s_",exmsg[x]);
%=    if(val)
%=       print(val);
%=    printf("\n;restarts:\n;[t]oplevel\n;[u]se_<form>instead\n;[r]eturn_<form>from_function\n");
%=    while(1) {
%=       lval_PTR_j;
%=       printf(";%d>",l);
%=       ex=lread(g);
%=       if(ex==8)
%=          longjmp(top_jmp,1);
%=       if(sp(ex)&& o2s(ex)[1]==84) {
%=          for(h=f,l=i=o2i(ex);i;i--) {
%=             if(!h[2])
%=                break;
%=             h=o2a(h[2]);
%=          }
%=       } else_if(ap(ex)&& o2a(ex)[1]==20) {
%=          switch(o2z(o2a(ex)[2])[0]) {
%=          case 'B':
%=             printf(";backtrace:\n");
%=             j=f;
%=             for(i=0;j;i++) {
%=                printf(";%d:",i);
%=                if(j[0]>>5==4) {
%=                   print(o2a(j[5])[6]);
%=                   printf("_");
%=                   print(j[4]);
%=                } printf("\n");
%=                if(!j[2])
%=                   break;
%=                j=o2a(j[2]);
%=             } break;
%=          case 'R':
%=             PTR_vp=eval(g,lread(g));
%=             return 1;
%=          case 'T':
%=             longjmp(top_jmp,1);
%=          case 'U':
%=             PTR_vp=eval(g,lread(g));
%=             return 0;
%=          }
%=       } else
%=          ep(h,ex);
%=    }
%- }
lisp500_fn([lval_evca(lval_PTR_f,lval_co)]):-true.
%- {
%=    lval_ex=car(co);
%=    lval_x=ex;
%=    int_m;
%=  ag:   xvalues=8;
%=    if(cp(ex)) {
%=       lval_fn=8;
%=       if(ap(car(ex))&& o2a(car(ex))[1]==20) {
%=          int_i=o2a(car(ex))[7]>>3;
%=          if(i>11&&i<34)
%=             return symi[i].fun(f,cdr(ex));
%=          fn=PTR_binding(f,car(ex),1,&m);
%=          if(m) {
%=             lval_PTR_g=f+1;
%=             for(ex=cdr(ex);ex;ex=cdr(ex))
%=                PTR++g=car(ex);
%=             x=ex=call(f,fn,g-f-1);
%=             set_car(co,ex);
%=             goto_ag;
%=          }}
%=  st:      if(fn==8) {
%=          if(dbgr(f,1,car(ex),&fn))
%=             return fn;
%=          else
%=             goto_st;
%=       } ex=cdr(ex);
%=       ex=call(f,fn,map_eval(f,ex));
%=    } else_if(ap(ex)&& o2a(ex)[1]==20) {
%=       ex=PTR_binding(f,ex,0,&m);
%=       if(m) {
%=          x=ex;
%=          set_car(co,ex);
%=          goto_ag;
%=       }
%=       if(ex==8)
%=          dbgr(f,0,x,&ex);
%=    } return ex==-8?o2a(x)[4]:ex;
%- }
lisp500_fn([int_getnws()]):-true.
%- {
%=    int_c;
%=    do
%=       c=getc(ins);
%=    while(isspace(c));
%=    return c;
%- }
lisp500_fn([lval_read_list(lval_PTR_f)]):-true.
%- {
%=    int_c;
%=    NF(1)T=0;
%=    c=getnws();
%=    if(c==')')
%=       return 0;
%=    if(c=='.') {
%=       lval_r=lread(g);
%=       getnws();
%=       return r;
%=    }
%=    ungetc(c,ins);
%=    T=lread(g);
%=    return cons(g,T,read_list(g));
%- }
lisp500_fn([lval_read_stri`ng_list(lval_PTR_g)]):-true.
%- {
%=    int c=getc(ins);
%=    if(c=='\"')
%=       return 0;
%=    if(c=='\\')
%=       c=getc(ins);
%=    return cons(g,(c_<<5)|24,read_string_list(g));
%- }
lisp500_fn([unsigned_hash(lval_s)]):-true.
%- {
%=    unsigned_char_PTR_z=o2z(s);
%=    unsigned_i=0,h=0,g;
%=    while(i<o2s(s)[0]/64-4) {
%=       h=(h_<<4)+z[i++];
%=       g=h &0xf0000000;
%=       if(g)
%=          h=h_^(g>>24)^g;
%=    }
%=    return h;
%- }
lisp500_fn([lval_lhash(lval_PTR_f)]):-true.
%- {
%=    return d2o(f,hash(f[1]));
%- }
lisp500_fn([lval_is(lval_PTR_g,lval_p,lval_s)]):-true.
%- {
%=    int_h=hash(s)%1021;
%=    int_i=3;
%=    lval_m;
%=    for(;i<5;i++) {
%=       m=o2a(o2a(p)[i])[2+h];
%=       for(;m;m=cdr(m)) {
%=          lval_y=car(m);
%=          if(string_equal(o2a(y)[2],s))
%=             return o2a(y)[7]?y_:0;
%=       }
%=    }
%=    m=ma(g,9,20,s,0,8,8,8,-8,16,p,0);
%=    if(p==kwp)
%=       o2a(m)[4]=m;
%=    o2a(o2a(p)[3])[2+h]=cons(g,m,o2a(o2a(p)[3])[2+h]);
%=    return m;
%- }
lisp500_fn([lval_read_symbol(lval_PTR_g)]):-true.
%- {
%=    int c=getc(ins);
%=    if(isspace(c)||c==')'||c==EOF) {
%=       if(c_!=EOF)
%=          ungetc(c,ins);
%=       return 0;
%=    } if(c>96&&c<123)
%=       c-=32;
%=    return cons(g,(c_<<5)|24,read_symbol(g));
%- }
lisp500_fn([lval_list2(lval_PTR_g,int_a)]):-true.
%- {
%=    return l2(g,symi[a].sym,lread(g));
%- }
lisp500_fn([lval_lread(lval_PTR_g)]):-true.
%- {
%=    int c=getnws();
%=    if(c==EOF)
%=       return 8;
%=    if(c=='(')
%=       return read_list(g);
%=    if(c=='\"')
%=       return stringify(g,read_string_list(g));
%=    if(c=='\'')
%=       return list2(g,12);
%=    if(c=='#') {
%=       c=getnws();
%=       if(c=='\'')
%=          return list2(g,20);
%=       return 0;
%=    } if(c=='`')
%=       return list2(g,38);
%=    if(c==',') {
%=       c=getnws();
%=       if(c=='@')
%=          return list2(g,40);
%=       ungetc(c,ins);
%=       return list2(g,39);
%=    } ungetc(c,ins);
%=    if(isdigit(c)) {
%=       double_d;
%=       fscanf(ins,"%lf",&d);
%=       return d2o(g,d);
%=    } if(c==':')
%=       getnws();
%=    return is(g,c==':'?kwp:pkg,stringify(g,read_symbol(g)));
%- }
lisp500_fn([lval_strf(lval_PTR_f,const_char_PTR_s)]):-true.
%- {
%=    int_j=strlen(s);
%=    lval_PTR_str=ms0(f,j);
%=    str[1]=20;
%=    for(j++;j;j--)
%=       ((char_PTR)str)[7+j]=s[j-1];
%=    return s2o(str);
%- }
lisp500_fn([lval_mkv(lval_PTR_f)]):-true.
%- {
%=    int_i=2;
%=    lval_PTR_r=ma0(f,1021);
%=    r[1]=116;
%=    while(i_<1023)
%=       r[i++]=0;
%=    return a2o(r);
%- }
lisp500_fn([lval_mkp(lval_PTR_f,const_char_PTR_s0,const_char_PTR_s1)]):-true.
%- {
%=    return ma(f,6,180,
%=       l2(f,strf(f,s0),strf(f,s1)),mkv(f),mkv(f),0,0,0);
%- }
% #if_0
lisp500_fn([lval_fr(lval_PTR_o,lval_PTR_p,lval_PTR_s,lval_PTR_c,lval_PTR_b,lval_x)]):-true.
%- {
%=    int_t;
%=    if(!(x &3))
%=       return x;
%=    t=(x>>30)&3;
%=    x &=0x3fffffff;
%=    switch(t) {
%=    case 0:
%=       return sp(x)?(lval)o+x_:(lval)b+x;
%=    case 1:
%=       return c[x/4];
%=    case 2:
%=       return s[x/4];
%=    default:
%=       return p[x/4];
%=    }
%- }
lisp500_fn([X_lval_fasr(lval_PTR_f,lval_PTR_p,int_pz,lval_PTR_s,lval_PTR_sp,int_sz,lval_PTR_c,int_cz,lval_PTR_v,int_vz,lval_PTR_o,int_oz,lval_PTR_PTR_rv,lval_PTR_PTR_ro)]):-true.
%- {
%=    lval_PTR_x,PTR_y;
%=    int_i,l,j;
%=    lval_pc,nc;
%=    y=ma0(f,oz-2);
%=    memcpy(y,o,4PTR_oz);
%=    for(i=0;i<pz;i++)
%=       for(pc=o2a(symi[81].sym)[4];pc;pc=cdr(pc))
%=          for(nc=o2a(car(pc))[2];nc;nc=cdr(nc))
%=             if(string_equal(car(nc),s2o(y+p[i]))) {
%=                p[i]=car(pc);
%=                break;
%=             }
%=    for(i=0;i<sz;i++)
%=       s[i]=is(f,p[sp[i]],s2o(y+s[i]));
%=    for(i=0;i<cz;i++)
%=       c[i]=o2a(s[c[i]])[3];
%=    x=ma0(f,vz-2);
%=    memcpy(x,v,4PTR_vz);
%=    for(i=0;i<vz;i+=((l+3)&~1))
%=       if(x[i+1] &4) {
%=          l=x[i]>>8;
%=          x[i+1]=fr(y,p,s,c,x,x[i+1]-4)+4;
%=          for(j=0;j<l;j++)
%=             x[i+j+2]=fr(y,p,s,c,x,x[i+j+2]);
%=       } else{
%=          l=0;
%=          x[i]=fr(y,p,s,c,x,x[i]);
%=          x[i+1]=fr(y,p,s,c,x,x[i+1]);
%=       } PTR_rv=x;
%=    PTR_ro=y;
%- }
% #endif
% #ifdef_WIN32
lisp500_fn([lval_lrp(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    STARTUPINFO_si={0};
%=    PROCESS_INFORMATION_pi={0};
%=    si.cb=sizeof(si);
%=    if(CreateProcess(o2z(f[1]),o2z(f[2]),NULL,NULL,FALSE,
%=          0,NULL,NULL,&si,&pi))
%=       return TRUE;
%=    return 0;
%- }
% #else
lisp500_fn([lval_lrp(lval_PTR_f,lval_PTR_h)]):-true.
%- {
%=    pidtp;
%=    int_r;
%=    p=fork();
%=    if(p)
%=       waitpid(p,&r,0);
%=    else{
%=       int_i=0;
%=       char_PTR_PTR_v=malloc((h-f-1)PTR_sizeof(char_PTR));
%=       for(;i<h-f-2;i++)
%=          v[i]=o2z(f[i+2]);
%=       v[i]=0;
%=       execv(o2z(f[1]),v);
%=    } return d2o(f,r);
%- }
% #endif
lisp500_fn([int_main(int_argc,char_PTR_argv_ARRAY)]):-true.
%- {
%=    lval_PTR_g;
%=    int_i;
%=    lval_sym;
%=    memory_size=4PTR2048PTR1024;
%=    memory=malloc(memory_size);
%=    memf=memory;
%=    memset(memory,0,memory_size);
%=    memf[0]=0;
%=    memf[1]=memory_size/4;
%=    stack=malloc(256PTR1024);
%=    memset(stack,0,256PTR1024);
%=    g=stack+5;
%=    pkg=mkp(g,"CL","COMMON-LISP");
%=    for(i=0;i<88;i++) {
%=       sym=is(g,pkg,strf(g,symi[i].name));
%=       if(i_<10)
%=          o2a(sym)[4]=sym;
%=       ins=stdin;
%=       symi[i].sym=sym;
%=       if(symi[i].fun)
%=          o2a(sym)[5]=ma(g,5,212,ms(g,3,212,symi[i].fun,0,-1),0,0,0,sym);
%=       if(symi[i].setfun)
%=          o2a(sym)[6]=ma(g,5,212,ms(g,3,212,symi[i].setfun,0,-1),8,0,0,sym);
%=       o2a(sym)[7]=i_<<3;
%=    }
%=    kwp=mkp(g,"","KEYWORD");
%=    o2a(symi[81].sym)[4]=pkgs=l2(g,kwp,pkg);
% #ifdef_WIN32
%=    o2a(symi[78].sym)[4]=ms(g,3,116,1,GetStdHandle(STD_INPUT_HANDLE),0,0);
%=    o2a(symi[79].sym)[4]=ms(g,3,116,1,GetStdHandle(STD_OUTPUT_HANDLE),TRUE,0);
%=    o2a(symi[80].sym)[4]=ms(g,3,116,1,GetStdHandle(STD_ERROR_HANDLE),TRUE,0);
% #else
%=    o2a(symi[78].sym)[4]=ms(g,3,116,1,0,0,0);
%=    o2a(symi[79].sym)[4]=ms(g,3,116,1,1,TRUE,0);
%=    o2a(symi[80].sym)[4]=ms(g,3,116,1,2,TRUE,0);
% #endif
%=    for(i=1;i<argc;i++)
%=       load(g,argv[i]);
%=    setjmp(top_jmp);
%=    do
%=       printf("?");
%=    while(ep(g,lread(g)));
%=    return 0;
%- }
lisp500_array([struct_symbol_init_symi_ARRAY,[["NIL"],["T"],["&REST"],["&BODY"],["&OPTIONAL"],["&KEY"],["&WHOLE"],["&ENVIRONMENT"],["&AUX"],["&ALLOW-OTHER-KEYS"],
  ["DECLARE",eval_declare,-1],["SPECIAL"],["QUOTE",eval_quote,1],["LET",eval_let,-2],
  ["LET_PTR_",eval_letm,-2],["FLET",eval_flet,-2],["LABELS",eval_labels,-2],
  ["MACROLET",eval_macrolet,-2],["SYMBOL-MACROLET",eval_symbol_macrolet,-2],["SETQ",eval_setq,2],
  ["FUNCTION",eval_function,1],["TAGBODY",eval_tagbody,-1],["GO",eval_go,1],
  ["BLOCK",eval_block,-2],["RETURN-FROM",eval_return from,2],["CATCH",eval_catch,-2],
  ["THROW",eval_throw,-2],["UNWIND-PROTECT",eval_unwind_protect,-2],["IF",eval_if,-3],
  ["MULTIPLE-VALUE-CALL",eval_multiple_value_call,-2],["MULTIPLE-VALUE-PROG1",eval_multiple_value_prog1,-2],
  ["PROGN",eval_body,-1],["PROGV",eval_progv,-3],["_SETF",eval_setf,2],
  ["FINISH-FILE-STREAM",lfinish_fs,1],["MAKEI",lmakei,-3],["DPB",ldpb,3],
  ["LDB",lldb,2],["BACKQUOTE"],["UNQUOTE"],["UNQUOTE-SPLICING"],["IBOUNDP",liboundp,2],["LISTEN-FILE-STREAM",llisten_fs,1],
  ["LIST",llist,-1],["VALUES",lvalues,-1],["FUNCALL",lfuncall,-2],
  ["APPLY",lapply,-2],["EQ",leq,2],["CONS",lcons,2],
  ["CAR",lcar,1,setfcar,2],["CDR",lcdr,1,setfcdr,2],["=",lequ,-2],
  ["<",lless,-2],["+",lplus,-1],["-",lminus,-2],
  ["_PTR_",ltimes,-1],["/",ldivi,-2],["MAKE-FILE-STREAM",lmake_fs,2],
  ["HASH",lhash,1],["IERROR"],["GENSYM",lgensym,0],["STRING",lstring,-1],
  ["FASL",lfasl,1],["MAKEJ",lmakej,2],["MAKEF",lmakef,0],
  ["FREF",lfref,1],["PRINT",lprint,1],["GC",gc,0],["CLOSE-FILE-STREAM",lclose_fs,1],
  ["IVAL",lival,1],["FLOOR",lfloor,-2],["READ-FILE-STREAM",lread_fs,3],
  ["WRITE-FILE-STREAM",lwrite_fs,4],["LOAD",lload,1],["IREF",liref,2,setfiref,3],
  ["LAMBDA"],["CODE-CHAR",lcode_char,1],["CHAR-CODE",lchar_code,1],["_PTR_STANDARD-INPUT_PTR_"],["_PTR_STANDARD-OUTPUT_PTR_"],["_PTR_ERROR-OUTPUT_PTR_"],["_PTR_PACKAGES_PTR_"],["STRING=",lstring_equal,2],
  ["IMAKUNBOUND",limakunbound,2],["EVAL",leval,-2],["JREF",ljref,2,setfjref,3],
  ["RUN-PROGRAM",lrp,-2],["UNAME",luname,0]]]).


:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
:- set_prolog_flag(double_quotes, codes). 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:-dynamic(user:mpred_prop/2).
:-multifile(user:mpred_prop/2).
:- use_module('../../../src_lib/logicmoo_util/logicmoo_util_all.pl').

parsing(String, Expr) :- string(String),!,string_codes(String,Codes),phrase(expressions(Expr), Codes).
parsing(String, Expr) :- phrase(expressions(Expr), String).

expressions([E|Es]) -->
    ws, expression(E), ws,
    !, % single solution: longest input match
    expressions(Es).
expressions([]) --> [].

ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% A number N is represented as n(N), a symbol S as s(S).

expression(s(A))         --> symbol(Cs), { atom_codes(A, Cs) }.
expression(n(N))         --> number(Cs), { number_codes(N, Cs) }.
expression(List)         --> "(", expressions(List), ")".
expression([s(quote),Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

symbol([A|As]) -->
    [A],
    { memberchk(A, "+/-*><=") ; code_type(A, alpha) },
    symbolr(As).

symbolr([A|As]) -->
    [A],
    { memberchk(A, "+/-*><=") ; code_type(A, alnum) },
    symbolr(As).
symbolr([]) --> [].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

codelist_to_forms(AsciiCodesList,FormsOut):-
    parsing(AsciiCodesList, Forms0),    
    compile_all(Forms0, FormsOut),!.

run(Program, Values) :-
    parsing(Program, Forms0),    
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).

unfunc(s(S), S).
unfunc(t, t).
unfunc(n(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).

fold([], _, V, n(V)).
fold([n(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).

compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

compile(F0, F) :-
    (   F0 = n(_)   -> F = F0
    ;   F0 = s(t)   -> F = t
    ;   F0 = s(nil) -> F = []
    ;   F0 = s(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = [s(quote),Arg] -> F = [quote,Arg]
    ;   F0 = [s(setq),s(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = [s(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = [s(defun),s(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = [s(Op)|Args0] -> compile_all(Args0, Args), F = [user(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval(n(N), n(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval(s(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, [n(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], s(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval(user(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate user:goal_truth(0,*,*,*).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.

bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).

run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).

if_script_file_time(X):-if_startup_script(time(X)).

% Append:
    :- if_script_file_time(run("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].
    

% Fibonacci, naive version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].
    

% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].
    

% Fibonacci, iterative version:
    :- if_script_file_time(run("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 34,233 inferences, 0.010 CPU in 0.010 seconds (98% CPU, 3423300 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].
    

% Higher-order programming and eval:
    :- if_startup_script(run("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].
 

