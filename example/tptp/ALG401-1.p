%------------------------------------------------------------------------------
% File     : ALG401-1 : TPTP v7.4.0. Released v4.1.0.
% Domain   : General Algebra
% Problem  : Fundamental theorem of algebra 0689_50
% Version  : Especial.
% English  :

% Refs     : [Nip10] Nipkow (2010), Email to Geoff Sutcliffe
%          : [BN10]  Boehme & Nipkow (2010), Sledgehammer: Judgement Day
% Source   : [Nip10]
% Names    : Fundamental_Theorem_Algebra-0689_50 [Nip10]

% Status   : Unsatisfiable
% Rating   : 0.53 v7.4.0, 0.41 v7.3.0, 0.42 v7.1.0, 0.33 v7.0.0, 0.53 v6.3.0, 0.45 v6.2.0, 0.50 v6.1.0, 0.86 v6.0.0, 0.80 v5.5.0, 0.90 v5.3.0, 0.94 v5.2.0, 0.88 v5.0.0, 0.86 v4.1.0
% Syntax   : Number of clauses     : 1068 ( 158 non-Horn; 169 unit; 678 RR)
%            Number of atoms       : 3000 ( 638 equality)
%            Maximal clause size   :    6 (   3 average)
%            Number of predicates  :   70 (   0 propositional; 1-5 arity)
%            Number of functors    :   34 (  10 constant; 0-3 arity)
%            Number of variables   : 2778 ( 140 singleton)
%            Maximal term depth    :    7 (   2 average)
% SPC      : CNF_UNS_RFO_SEQ_NHN

% Comments :
%------------------------------------------------------------------------------
cnf(cls_poly__eq__0__iff__dvd_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Opoly(V_p,V_c,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_OpCons(c_HOL_Ouminus__class_Ouminus(V_c,T_a),c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a),V_p,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_synthetic__div__unique__lemma_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(V_c,V_p,T_a) != c_Polynomial_OpCons(V_a,V_p,T_a)
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_q_I1_J_0,axiom,
    ( c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(v_q____,tc_Complex_Ocomplex) = c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(v_pa____,tc_Complex_Ocomplex) )).

cnf(cls_minus__divide__left_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) )).

cnf(cls_minus__divide__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_frac__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_x,V_z,T_a),c_HOL_Oinverse__class_Odivide(V_y,V_w,T_a),T_a)
    | ~ c_lessequals(V_w,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_w,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_mult__poly__add__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_p,V_q,tc_Polynomial_Opoly(T_a)),V_r,tc_Polynomial_Opoly(T_a)) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_p,V_r,tc_Polynomial_Opoly(T_a)),c_HOL_Otimes__class_Otimes(V_q,V_r,tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_real__mult__assoc_0,axiom,
    ( c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_z1,V_z2,tc_RealDef_Oreal),V_z3,tc_RealDef_Oreal) = c_HOL_Otimes__class_Otimes(V_z1,c_HOL_Otimes__class_Otimes(V_z2,V_z3,tc_RealDef_Oreal),tc_RealDef_Oreal) )).

cnf(cls_dvd__neg__div_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Ouminus__class_Ouminus(V_x,T_a),V_y,T_a) = c_HOL_Ouminus__class_Ouminus(c_Divides_Odiv__class_Odiv(V_x,V_y,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_y,V_x,T_a) )).

cnf(cls_dvd__div__neg_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Odiv(V_x,c_HOL_Ouminus__class_Ouminus(V_y,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_Divides_Odiv__class_Odiv(V_x,V_y,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_y,V_x,T_a) )).

cnf(cls_smult__dvd__iff_4,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(V_a,V_p,T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_divide__right__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),c_HOL_Oinverse__class_Odivide(V_a,V_c,T_a),T_a)
    | ~ c_lessequals(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_divide__right__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_c,T_a),c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_xt1_I11_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | V_a = V_b
    | ~ c_lessequals(V_b,V_a,T_a) )).

cnf(cls_xt1_I12_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_lessequals(V_b,V_a,T_a)
    | V_a = V_b )).

cnf(cls_linorder__antisym__conv2_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | V_x = V_y
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_linorder__antisym__conv1_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | V_x = V_y
    | ~ c_lessequals(V_x,V_y,T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_order__neq__le__trans_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_lessequals(V_a,V_b,T_a)
    | V_a = V_b )).

cnf(cls_order__le__neq__trans_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | V_a = V_b
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_real__less__def_2,axiom,
    ( c_HOL_Oord__class_Oless(V_x,V_y,tc_RealDef_Oreal)
    | V_x = V_y
    | ~ c_lessequals(V_x,V_y,tc_RealDef_Oreal) )).

cnf(cls_order__le__less_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | V_x = V_y
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_order__less__le_2,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | V_x = V_y
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_le__eqI_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) != c_HOL_Ominus__class_Ominus(V_x_H,V_y_H,T_a)
    | c_lessequals(V_y_H,V_x_H,T_a)
    | ~ c_lessequals(V_y,V_x,T_a) )).

cnf(cls_le__eqI_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) != c_HOL_Ominus__class_Ominus(V_x_H,V_y_H,T_a)
    | c_lessequals(V_y,V_x,T_a)
    | ~ c_lessequals(V_y_H,V_x_H,T_a) )).

cnf(cls_less__eqI_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) != c_HOL_Ominus__class_Ominus(V_x_H,V_y_H,T_a)
    | c_HOL_Oord__class_Oless(V_x_H,V_y_H,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_less__eqI_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) != c_HOL_Ominus__class_Ominus(V_x_H,V_y_H,T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x_H,V_y_H,T_a) )).

cnf(cls_pdivmod__rel__by__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_q = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | ~ c_Polynomial_Opdivmod__rel(V_x,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_q,V_r,T_a) )).

cnf(cls_pdivmod__rel__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_r = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | ~ c_Polynomial_Opdivmod__rel(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_y,V_q,V_r,T_a) )).

cnf(cls_pdivmod__rel__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_q = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | ~ c_Polynomial_Opdivmod__rel(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_y,V_q,V_r,T_a) )).

cnf(cls_pdivmod__rel__def_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_x,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_q,V_r,T_a)
    | V_q = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_monom__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Omonom(V_a,c_HOL_Ozero__class_Ozero(tc_nat),T_a) = c_Polynomial_OpCons(V_a,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_divide__nonpos__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__nonneg__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_zero__less__divide__iff_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__less__divide__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__less__divide__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__less__divide__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_monom__eq__0__iff_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Omonom(V_a,V_n,T_a) != c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_poly__mod__mult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Omod(V_x,c_HOL_Otimes__class_Otimes(V_y,V_z,tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_y,c_Divides_Odiv__class_Omod(c_Divides_Odiv__class_Odiv(V_x,V_y,tc_Polynomial_Opoly(T_a)),V_z,tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)),c_Divides_Odiv__class_Omod(V_x,V_y,tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_pCons__eq__iff_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_OpCons(V_a,V_p,T_a) != c_Polynomial_OpCons(V_b,V_q,T_a)
    | V_a = V_b )).

cnf(cls_pCons__eq__iff_1,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_OpCons(V_a,V_p,T_a) != c_Polynomial_OpCons(V_b,V_q,T_a)
    | V_p = V_q )).

cnf(cls_realpow__num__eq__if_0,axiom,
    ( c_Power_Opower__class_Opower(V_m,c_HOL_Ozero__class_Ozero(tc_nat),tc_RealDef_Oreal) = c_HOL_Oone__class_Oone(tc_RealDef_Oreal) )).

cnf(cls_pdivmod__rel__by__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_r = V_x
    | ~ c_Polynomial_Opdivmod__rel(V_x,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_q,V_r,T_a) )).

cnf(cls_neg__le__0__iff__le_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_neg__le__0__iff__le_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_div__mult__div__if__dvd_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_x,V_y,T_a),c_Divides_Odiv__class_Odiv(V_w,V_z,T_a),T_a) = c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(V_x,V_w,T_a),c_HOL_Otimes__class_Otimes(V_y,V_z,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_z,V_w,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_y,V_x,T_a) )).

cnf(cls_leading__coeff__0__iff_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Ocoeff(V_p,c_Polynomial_Odegree(V_p,T_a),T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_dvd__trans_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_b,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__refl_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,V_a,T_a) )).

cnf(cls_linorder__neqE__ordered__idom_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(V_y,V_x,T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | V_x = V_y )).

cnf(cls_not__less__iff__gr__or__eq_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | V_x = V_y
    | c_HOL_Oord__class_Oless(V_y,V_x,T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_linorder__antisym__conv3_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | V_x = V_y
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | c_HOL_Oord__class_Oless(V_y,V_x,T_a) )).

cnf(cls_linorder__less__linear_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_HOL_Oord__class_Oless(V_y,V_x,T_a)
    | V_x = V_y
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_pdivmod__rel__unique_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_r1 = V_r2
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q2,V_r2,T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q1,V_r1,T_a) )).

cnf(cls_pdivmod__rel__unique_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_q1 = V_q2
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q2,V_r2,T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q1,V_r1,T_a) )).

cnf(cls_order__antisym__conv_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | V_x = V_y
    | ~ c_lessequals(V_x,V_y,T_a)
    | ~ c_lessequals(V_y,V_x,T_a) )).

cnf(cls_order__eq__iff_2,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | V_x = V_y
    | ~ c_lessequals(V_y,V_x,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_linorder__neqE_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_HOL_Oord__class_Oless(V_y,V_x,T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | V_x = V_y )).

cnf(cls_order__antisym_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | V_x = V_y
    | ~ c_lessequals(V_y,V_x,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_real__le__antisym_0,axiom,
    ( V_z = V_w
    | ~ c_lessequals(V_w,V_z,tc_RealDef_Oreal)
    | ~ c_lessequals(V_z,V_w,tc_RealDef_Oreal) )).

cnf(cls_compl__eq__compl__iff_0,axiom,
    ( ~ class_Lattices_Oboolean__algebra(T_a)
    | c_HOL_Ouminus__class_Ouminus(V_x,T_a) != c_HOL_Ouminus__class_Ouminus(V_y,T_a)
    | V_x = V_y )).

cnf(cls_neg__equal__iff__equal_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(V_a,T_a) != c_HOL_Ouminus__class_Ouminus(V_b,T_a)
    | V_a = V_b )).

cnf(cls_one__poly__def_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oone__class_Oone(tc_Polynomial_Opoly(T_a)) = c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_neg__less__iff__less_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_b,T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_neg__less__iff__less_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_b,T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_le__divide__eq_8,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_divide__le__eq_8,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_offset__poly__eq__0__lemma_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Oplus__class_Oplus(c_Polynomial_Osmult(V_c,V_p,T_a),c_Polynomial_OpCons(V_a,V_p,T_a),tc_Polynomial_Opoly(T_a)) != c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_diff__divide__distrib_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Ominus__class_Ominus(c_HOL_Oinverse__class_Odivide(V_a,V_c,T_a),c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_divide_Odiff_0,axiom,
    ( ~ class_RealVector_Oreal__normed__field(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(V_x,V_y,T_a),V_ya,T_a) = c_HOL_Ominus__class_Ominus(c_HOL_Oinverse__class_Odivide(V_x,V_ya,T_a),c_HOL_Oinverse__class_Odivide(V_y,V_ya,T_a),T_a) )).

cnf(cls_add__is__0_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat) != c_HOL_Ozero__class_Ozero(tc_nat)
    | V_m = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_add__is__0_1,axiom,
    ( c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat) != c_HOL_Ozero__class_Ozero(tc_nat)
    | V_n = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_dvd__diff_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__ring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_x,c_HOL_Ominus__class_Ominus(V_y,V_z,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_x,V_z,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_x,V_y,T_a) )).

cnf(cls_mult__imp__le__div__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(V_z,c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_z,V_y,T_a),V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a) )).

cnf(cls_mult__imp__div__pos__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),V_z,T_a)
    | ~ c_lessequals(V_x,c_HOL_Otimes__class_Otimes(V_z,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a) )).

cnf(cls_le__divide__eq_6,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_poly__eq__0__iff__dvd_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Opoly(V_p,V_c,T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_OpCons(c_HOL_Ouminus__class_Ouminus(V_c,T_a),c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a),V_p,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_divide__less__eq_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a) )).

cnf(cls_divide__less__eq_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_less__divide__eq_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_less__divide__eq_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mod__minus__eq_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Ouminus__class_Ouminus(c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a),V_b,T_a) )).

cnf(cls_divide__neg__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__pos__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_zero__less__divide__iff_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__less__divide__iff_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_group__add__class_Odiff__0_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = c_HOL_Ouminus__class_Ouminus(V_a,T_a) )).

cnf(cls_offset__poly__single_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(c_Polynomial_OpCons(V_a,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),V_h,T_a) = c_Polynomial_OpCons(V_a,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_diff__def_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) = c_HOL_Oplus__class_Oplus(V_x,c_HOL_Ouminus__class_Ouminus(V_y,T_a),T_a) )).

cnf(cls_class__ring_Osub__add_0,axiom,
    ( ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) = c_HOL_Oplus__class_Oplus(V_x,c_HOL_Ouminus__class_Ouminus(V_y,T_a),T_a) )).

cnf(cls_diff__minus_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,V_b,T_a) = c_HOL_Oplus__class_Oplus(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_ab__diff__minus_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,V_b,T_a) = c_HOL_Oplus__class_Oplus(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_nonzero__minus__divide__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__dvd__div_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_b,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_Divides_Odiv__class_Odiv(V_b,V_a,T_a),c_Divides_Odiv__class_Odiv(V_c,V_a,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_div__dvd__div_1,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Divides_Odiv__class_Odiv(V_b,V_a,T_a),c_Divides_Odiv__class_Odiv(V_c,V_a,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_b,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__minus__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__ring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_x,V_y,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_x,c_HOL_Ouminus__class_Ouminus(V_y,T_a),T_a) )).

cnf(cls_dvd__minus__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ocomm__ring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_x,c_HOL_Ouminus__class_Ouminus(V_y,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_x,V_y,T_a) )).

cnf(cls_minus__dvd__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__ring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_x,V_y,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Ouminus__class_Ouminus(V_x,T_a),V_y,T_a) )).

cnf(cls_minus__dvd__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ocomm__ring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Ouminus__class_Ouminus(V_x,T_a),V_y,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_x,V_y,T_a) )).

cnf(cls_degree__linear__power_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Odegree(c_Power_Opower__class_Opower(c_Polynomial_OpCons(V_a,c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a),V_n,tc_Polynomial_Opoly(T_a)),T_a) = V_n )).

cnf(cls_add__monom_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_Polynomial_Omonom(V_a,V_n,T_a),c_Polynomial_Omonom(V_b,V_n,T_a),tc_Polynomial_Opoly(T_a)) = c_Polynomial_Omonom(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_n,T_a) )).

cnf(cls_divide__strict__right__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_c,T_a),c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_divide__strict__right__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_c,T_a),c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a) )).

cnf(cls_mod__diff__eq_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) )).

cnf(cls_divide__nonneg__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_divide__nonpos__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_nat__add__right__cancel_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_m,V_k,tc_nat) != c_HOL_Oplus__class_Oplus(V_n,V_k,tc_nat)
    | V_m = V_n )).

cnf(cls_nat__add__left__cancel_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_k,V_m,tc_nat) != c_HOL_Oplus__class_Oplus(V_k,V_n,tc_nat)
    | V_m = V_n )).

cnf(cls_natgb_Oadd__cancel_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_x,V_y,tc_nat) != c_HOL_Oplus__class_Oplus(V_x,V_z,tc_nat)
    | V_y = V_z )).

cnf(cls_pdivmod__rel__0_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_y,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_mult__monom_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(c_Polynomial_Omonom(V_a,V_m,T_a),c_Polynomial_Omonom(V_b,V_n,T_a),tc_Polynomial_Opoly(T_a)) = c_Polynomial_Omonom(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat),T_a) )).

cnf(cls_dvd__imp__mod__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_b,V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__eq__mod__eq__0_1,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_b,V_a,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_pCons__0__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_OpCons(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_divide__le__eq_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_divide__le__eq_7,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_le__divide__eq_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_le__divide__eq_7,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_coeff__add_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_Polynomial_Ocoeff(c_HOL_Oplus__class_Oplus(V_p,V_q,tc_Polynomial_Opoly(T_a)),V_n,T_a) = c_HOL_Oplus__class_Oplus(c_Polynomial_Ocoeff(V_p,V_n,T_a),c_Polynomial_Ocoeff(V_q,V_n,T_a),T_a) )).

cnf(cls_offset__poly__eq__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(V_p,V_h,T_a) != c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_neg__0__le__iff__le_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_neg__0__le__iff__le_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_coeff__monom_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Ocoeff(c_Polynomial_Omonom(V_a,V_x,T_a),V_x,T_a) = V_a )).

cnf(cls_mod__diff__left__eq_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),V_b,T_a),V_c,T_a) )).

cnf(cls_mod__diff__right__eq_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(V_a,c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) )).

cnf(cls_psize__def_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_pdivmod__rel__def_6,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_y,tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)),V_y,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_pdivmod__rel__def_5,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)),V_r,tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_r,T_a) )).

cnf(cls_leading__coeff__0__iff_1,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Ocoeff(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),c_Polynomial_Odegree(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mod__mod__trivial_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_Divides_Odiv__class_Omod(V_a,V_b,T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(V_a,V_b,T_a) )).

cnf(cls_le__minus__self__iff_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_le__minus__self__iff_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_minus__le__self__iff_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_a,T_a) )).

cnf(cls_minus__le__self__iff_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_less__eq__neg__nonpos_0,axiom,
    ( ~ class_OrderedGroup_Oordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_less__eq__neg__nonpos_1,axiom,
    ( ~ class_OrderedGroup_Oordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_neg__less__eq__nonneg_0,axiom,
    ( ~ class_OrderedGroup_Oordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_a,T_a) )).

cnf(cls_neg__less__eq__nonneg_1,axiom,
    ( ~ class_OrderedGroup_Oordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_degree__pCons__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Odegree(c_Polynomial_OpCons(V_a,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a) = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_coeff__1_1,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Ocoeff(c_HOL_Oone__class_Oone(tc_Polynomial_Opoly(T_a)),V_n,T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | V_n = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_psize__eq__0__iff_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(V_p,T_a) != c_HOL_Ozero__class_Ozero(tc_nat)
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_pdivmod__rel__by__0_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(V_x,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_x,T_a) )).

cnf(cls_degree__mult__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Odegree(c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Oplus__class_Oplus(c_Polynomial_Odegree(V_p,T_a),c_Polynomial_Odegree(V_q,T_a),tc_nat)
    | V_q = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_nat__add__commute_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat) = c_HOL_Oplus__class_Oplus(V_n,V_m,tc_nat) )).

cnf(cls_divide__le__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__le__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__le__0__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__le__0__iff_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mod__div__trivial_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_Divides_Odiv__class_Omod(V_a,V_b,T_a),V_b,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_pCons__eq__0__iff_1,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_OpCons(V_a,V_p,T_a) != c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_pCons__eq__0__iff_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_OpCons(V_a,V_p,T_a) != c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mod__minus__cong_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_a,V_b,T_a) != c_Divides_Odiv__class_Omod(V_a_H,V_b,T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Ouminus__class_Ouminus(V_a_H,T_a),V_b,T_a) )).

cnf(cls_div__poly__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Odiv(V_x,V_y,tc_Polynomial_Opoly(T_a)) = V_q
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q,V_r,T_a) )).

cnf(cls_add__pCons_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_Polynomial_OpCons(V_a,V_p,T_a),c_Polynomial_OpCons(V_b,V_q,T_a),tc_Polynomial_Opoly(T_a)) = c_Polynomial_OpCons(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Oplus__class_Oplus(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_div__power_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Power_Opower__class_Opower(c_Divides_Odiv__class_Odiv(V_x,V_y,T_a),V_n,T_a) = c_Divides_Odiv__class_Odiv(c_Power_Opower__class_Opower(V_x,V_n,T_a),c_Power_Opower__class_Opower(V_y,V_n,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_y,V_x,T_a) )).

cnf(cls_degree__monom__eq_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Odegree(c_Polynomial_Omonom(V_a,V_n,T_a),T_a) = V_n
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__poly__0__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_q,tc_Polynomial_Opoly(T_a)) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_mult__poly__0__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(V_p,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_mult__pCons__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(c_Polynomial_OpCons(V_a,V_p,T_a),V_q,tc_Polynomial_Opoly(T_a)) = c_HOL_Oplus__class_Oplus(c_Polynomial_Osmult(V_a,V_q,T_a),c_Polynomial_OpCons(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_mult__pCons__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(V_p,c_Polynomial_OpCons(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a)) = c_HOL_Oplus__class_Oplus(c_Polynomial_Osmult(V_a,V_p,T_a),c_Polynomial_OpCons(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_linorder__neq__iff_1,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_x,T_a) )).

cnf(cls_order__less__le_1,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_x,T_a) )).

cnf(cls_real__less__def_1,axiom,
    ( ~ c_HOL_Oord__class_Oless(V_x,V_x,tc_RealDef_Oreal) )).

cnf(cls_order__less__irrefl_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_x,T_a) )).

cnf(cls_less__le__not__le_2,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | c_lessequals(V_y,V_x,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_mod__diff__cong_0,axiom,
    ( ~ class_Divides_Oring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_b,V_c,T_a) != c_Divides_Odiv__class_Omod(V_b_H,V_c,T_a)
    | c_Divides_Odiv__class_Omod(V_a,V_c,T_a) != c_Divides_Odiv__class_Omod(V_a_H,V_c,T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Ominus__class_Ominus(V_a_H,V_b_H,T_a),V_c,T_a) )).

cnf(cls_le__iff__diff__le__0_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_le__iff__diff__le__0_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_order__le__less_1,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_lessequals(V_x,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_real__less__def_0,axiom,
    ( c_lessequals(V_x,V_y,tc_RealDef_Oreal)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,tc_RealDef_Oreal) )).

cnf(cls_order__less__imp__le_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_lessequals(V_x,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_zero__le__divide__iff_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__le__divide__iff_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_frac__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_x,V_z,T_a),c_HOL_Oinverse__class_Odivide(V_y,V_w,T_a),T_a)
    | ~ c_lessequals(V_w,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_w,T_a)
    | ~ c_lessequals(V_x,V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_coeff__pCons__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Ocoeff(c_Polynomial_OpCons(V_a,V_p,T_a),c_HOL_Ozero__class_Ozero(tc_nat),T_a) = V_a )).

cnf(cls_less__iff__diff__less__0_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_le__divide__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_divide__le__eq_6,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_divide__le__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a) )).

cnf(cls_natgb_Oadd__r0__iff_0,axiom,
    ( V_x != c_HOL_Oplus__class_Oplus(V_x,V_a,tc_nat)
    | V_a = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_add__eq__self__zero_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat) != V_m
    | V_n = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_pdivmod__rel__def_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_x = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_q,V_y,tc_Polynomial_Opoly(T_a)),V_r,tc_Polynomial_Opoly(T_a))
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q,V_r,T_a) )).

cnf(cls_dvd__mod__imp__dvd_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_k,V_m,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,V_n,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,c_Divides_Odiv__class_Omod(V_m,V_n,T_a),T_a) )).

cnf(cls_dvd__mod__iff_1,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_k,c_Divides_Odiv__class_Omod(V_m,V_n,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,V_m,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,V_n,T_a) )).

cnf(cls_dvd__mod__iff_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_k,V_m,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,c_Divides_Odiv__class_Omod(V_m,V_n,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,V_n,T_a) )).

cnf(cls_dvd__mod_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_k,c_Divides_Odiv__class_Omod(V_m,V_n,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,V_n,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_k,V_m,T_a) )).

cnf(cls_divide__le__0__iff_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__le__0__iff_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_coeff__linear__power_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Ocoeff(c_Power_Opower__class_Opower(c_Polynomial_OpCons(V_a,c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a),V_n,tc_Polynomial_Opoly(T_a)),V_n,T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_div__add_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Oplus__class_Oplus(V_x,V_y,T_a),V_z,T_a) = c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Odiv(V_x,V_z,T_a),c_Divides_Odiv__class_Odiv(V_y,V_z,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_z,V_y,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_z,V_x,T_a) )).

cnf(cls_neg__le__iff__le_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ouminus__class_Ouminus(V_b,T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_le__imp__neg__le_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ouminus__class_Ouminus(V_b,T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_degree__1_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Odegree(c_HOL_Oone__class_Oone(tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_monom__eq__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Omonom(c_HOL_Ozero__class_Ozero(T_a),V_n,T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_divide__left__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_c,V_a,T_a),c_HOL_Oinverse__class_Odivide(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_divide__left__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_c,V_a,T_a),c_HOL_Oinverse__class_Odivide(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_b,V_a,T_a) )).

cnf(cls_degree__offset__poly_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Odegree(c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(V_p,V_h,T_a),T_a) = c_Polynomial_Odegree(V_p,T_a) )).

cnf(cls_offset__poly__0_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_h,T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_real__mult__1_0,axiom,
    ( c_HOL_Otimes__class_Otimes(c_HOL_Oone__class_Oone(tc_RealDef_Oreal),V_z,tc_RealDef_Oreal) = V_z )).

cnf(cls_pdivmod__rel_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(V_x,V_y,c_Divides_Odiv__class_Odiv(V_x,V_y,tc_Polynomial_Opoly(T_a)),c_Divides_Odiv__class_Omod(V_x,V_y,tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_frac__less2_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_x,V_z,T_a),c_HOL_Oinverse__class_Odivide(V_y,V_w,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_w,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_w,T_a)
    | ~ c_lessequals(V_x,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_nonzero__minus__divide__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_neg__less__0__iff__less_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_neg__less__0__iff__less_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_offset__poly__pCons_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(c_Polynomial_OpCons(V_a,V_p,T_a),V_h,T_a) = c_HOL_Oplus__class_Oplus(c_Polynomial_Osmult(V_h,c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(V_p,V_h,T_a),T_a),c_Polynomial_OpCons(V_a,c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(V_p,V_h,T_a),T_a),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_add__is__0_2,axiom,
    ( c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(tc_nat),c_HOL_Ozero__class_Ozero(tc_nat),tc_nat) = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_real__mult__commute_0,axiom,
    ( c_HOL_Otimes__class_Otimes(V_z,V_w,tc_RealDef_Oreal) = c_HOL_Otimes__class_Otimes(V_w,V_z,tc_RealDef_Oreal) )).

cnf(cls_xt1_I10_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_z,V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(V_z,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_x,T_a) )).

cnf(cls_order__less__trans_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_coeff__monom_1,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Ocoeff(c_Polynomial_Omonom(V_a,V_m,T_a),V_n,T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | V_m = V_n )).

cnf(cls_nat__add__assoc_0,axiom,
    ( c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat),V_k,tc_nat) = c_HOL_Oplus__class_Oplus(V_m,c_HOL_Oplus__class_Oplus(V_n,V_k,tc_nat),tc_nat) )).

cnf(cls_nat__add__left__commute_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_x,c_HOL_Oplus__class_Oplus(V_y,V_z,tc_nat),tc_nat) = c_HOL_Oplus__class_Oplus(V_y,c_HOL_Oplus__class_Oplus(V_x,V_z,tc_nat),tc_nat) )).

cnf(cls_less__le__not__le_1,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | ~ c_lessequals(V_y,V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_not__leE_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | c_lessequals(V_y,V_x,T_a) )).

cnf(cls_linorder__antisym__conv2_1,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | ~ c_lessequals(V_x,V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_x,T_a) )).

cnf(cls_linorder__antisym__conv1_1,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_x,T_a)
    | c_lessequals(V_x,V_x,T_a) )).

cnf(cls_linorder__not__less_1,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | ~ c_lessequals(V_y,V_x,T_a) )).

cnf(cls_linorder__not__less_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_lessequals(V_y,V_x,T_a)
    | c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_linorder__not__le_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_HOL_Oord__class_Oless(V_y,V_x,T_a)
    | c_lessequals(V_x,V_y,T_a) )).

cnf(cls_linorder__not__le_1,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | ~ c_lessequals(V_x,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_x,T_a) )).

cnf(cls_zero__le__divide__iff_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__le__divide__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__le__divide__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__le__divide__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) )).

cnf(cls_eq__eqI_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_x,T_a) != c_HOL_Ominus__class_Ominus(V_x_H,V_y_H,T_a)
    | V_x_H = V_y_H )).

cnf(cls_eq__eqI_1,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_xa,V_y,T_a) != c_HOL_Ominus__class_Ominus(V_x,V_x,T_a)
    | V_xa = V_y )).

cnf(cls_mod__poly__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Omod(V_x,V_y,tc_Polynomial_Opoly(T_a)) = V_r
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q,V_r,T_a) )).

cnf(cls_minus__divide__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a) )).

cnf(cls_divide_Ominus_0,axiom,
    ( ~ class_RealVector_Oreal__normed__field(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ouminus__class_Ouminus(V_x,T_a),V_y,T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a) )).

cnf(cls_coeff__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Ocoeff(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_n,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_dvd__div__mult_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_b,V_a,T_a),V_c,T_a) = c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),V_a,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__iff__poly__eq__0_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Opoly(V_p,c_HOL_Ouminus__class_Ouminus(V_c,T_a),T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_OpCons(V_c,c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a),V_p,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_dvd__iff__poly__eq__0_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Opoly(V_p,c_HOL_Ouminus__class_Ouminus(V_c,T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_OpCons(V_c,c_Polynomial_OpCons(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a),T_a),V_p,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_coeff__mult__degree__sum_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Ocoeff(c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),c_HOL_Oplus__class_Oplus(c_Polynomial_Odegree(V_p,T_a),c_Polynomial_Odegree(V_q,T_a),tc_nat),T_a) = c_HOL_Otimes__class_Otimes(c_Polynomial_Ocoeff(V_p,c_Polynomial_Odegree(V_p,T_a),T_a),c_Polynomial_Ocoeff(V_q,c_Polynomial_Odegree(V_q,T_a),T_a),T_a) )).

cnf(cls_degree__0_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Odegree(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_divide__less__0__iff_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__less__0__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__less__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__less__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_pdivmod__rel__mult_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(V_x,c_HOL_Otimes__class_Otimes(V_y,V_z,tc_Polynomial_Opoly(T_a)),V_q_H,c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_y,V_r_H,tc_Polynomial_Opoly(T_a)),V_r,tc_Polynomial_Opoly(T_a)),T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_q,V_z,V_q_H,V_r_H,T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q,V_r,T_a) )).

cnf(cls_real__mult__is__one_1,axiom,
    ( c_HOL_Otimes__class_Otimes(c_HOL_Oone__class_Oone(tc_RealDef_Oreal),c_HOL_Oone__class_Oone(tc_RealDef_Oreal),tc_RealDef_Oreal) = c_HOL_Oone__class_Oone(tc_RealDef_Oreal) )).

cnf(cls_dvd__mult__div__cancel_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_Divides_Odiv__class_Odiv(V_b,V_a,T_a),T_a) = V_b
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__div__mult__self_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_b,V_a,T_a),V_a,T_a) = V_b
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_diff__minus__eq__add_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Oplus__class_Oplus(V_a,V_b,T_a) )).

cnf(cls_poly__offset_0,axiom,
    ( c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(v_sko__Fundamental__Theorem__Algebra__Mirabelle__Xpoly__offset__1(V_a,V_p),tc_Complex_Ocomplex) = c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(V_p,tc_Complex_Ocomplex) )).

cnf(cls_add__poly__code_I1_J_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_q,tc_Polynomial_Opoly(T_a)) = V_q )).

cnf(cls_add__poly__code_I2_J_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_p,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) = V_p )).

cnf(cls_natgb_Oadd__r0__iff_1,axiom,
    ( V_x = c_HOL_Oplus__class_Oplus(V_x,c_HOL_Ozero__class_Ozero(tc_nat),tc_nat) )).

cnf(cls_Nat_Oadd__0__right_0,axiom,
    ( c_HOL_Oplus__class_Oplus(V_m,c_HOL_Ozero__class_Ozero(tc_nat),tc_nat) = V_m )).

cnf(cls_plus__nat_Oadd__0_0,axiom,
    ( c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(tc_nat),V_n,tc_nat) = V_n )).

cnf(cls_le__divide__eq_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_le__divide__eq_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_divide__le__eq_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_divide__le__eq_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a) )).

cnf(cls_minus__equation__iff_1,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = V_b )).

cnf(cls_equation__minus__iff_1,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | V_a = c_HOL_Ouminus__class_Ouminus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_equation__minus__iff_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | V_b = c_HOL_Ouminus__class_Ouminus(c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_double__compl_0,axiom,
    ( ~ class_Lattices_Oboolean__algebra(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Ouminus__class_Ouminus(V_x,T_a),T_a) = V_x )).

cnf(cls_minus__minus_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) = V_a )).

cnf(cls_minus__diff__eq_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),T_a) = c_HOL_Ominus__class_Ominus(V_b,V_a,T_a) )).

cnf(cls_add__less__le__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__cancel__ab__semigroup__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(V_c,V_d,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_add__le__less__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__cancel__ab__semigroup__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,V_d,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_minus__less__iff_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_b,T_a),V_a,T_a) )).

cnf(cls_minus__less__iff_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_b,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) )).

cnf(cls_less__minus__iff_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_less__minus__iff_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_minus__le__iff_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ouminus__class_Ouminus(V_b,T_a),V_a,T_a) )).

cnf(cls_minus__le__iff_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ouminus__class_Ouminus(V_b,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) )).

cnf(cls_le__minus__iff_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_le__minus__iff_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(V_b,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_eq__divide__eq_2,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Ozero__class_Ozero(T_a) = c_HOL_Oinverse__class_Odivide(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_neg__equal__zero_1,axiom,
    ( ~ class_OrderedGroup_Oordered__ab__group__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_minus__zero_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_neg__0__equal__iff__equal_1,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ozero__class_Ozero(T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_dvd__0__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mod__self_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_a,V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_less__iff__diff__less__0_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mod__mod__cancel_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_Divides_Odiv__class_Omod(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(V_a,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_c,V_b,T_a) )).

cnf(cls_poly__dvd__antisym_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Ocoeff(V_p,c_Polynomial_Odegree(V_p,T_a),T_a) != c_Polynomial_Ocoeff(V_q,c_Polynomial_Odegree(V_q,T_a),T_a)
    | V_p = V_q
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_q,V_p,tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_Lim_Ominus__diff__minus_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),T_a) )).

cnf(cls_poly__div__mult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Odiv(V_x,c_HOL_Otimes__class_Otimes(V_y,V_z,tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) = c_Divides_Odiv__class_Odiv(c_Divides_Odiv__class_Odiv(V_x,V_y,tc_Polynomial_Opoly(T_a)),V_z,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_divide__le__eq_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a) )).

cnf(cls_divide__le__eq_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_le__divide__eq_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_le__divide__eq_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_divide__neg__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__pos__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_divide__less__0__iff_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_divide__less__0__iff_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_order__eq__refl_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_lessequals(V_x,V_x,T_a) )).

cnf(cls_order__eq__iff_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_lessequals(V_x,V_x,T_a) )).

cnf(cls_real__le__trans_0,axiom,
    ( c_lessequals(V_i,V_k,tc_RealDef_Oreal)
    | ~ c_lessequals(V_j,V_k,tc_RealDef_Oreal)
    | ~ c_lessequals(V_i,V_j,tc_RealDef_Oreal) )).

cnf(cls_real__le__refl_0,axiom,
    ( c_lessequals(V_w,V_w,tc_RealDef_Oreal) )).

cnf(cls_order__le__less__trans_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_z,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_order__less__le__trans_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_HOL_Oord__class_Oless(V_x,V_z,T_a)
    | ~ c_lessequals(V_y,V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_order__trans_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | c_lessequals(V_x,V_z,T_a)
    | ~ c_lessequals(V_y,V_z,T_a)
    | ~ c_lessequals(V_x,V_y,T_a) )).

cnf(cls_xt1_I8_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_z,V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(V_z,V_y,T_a)
    | ~ c_lessequals(V_y,V_x,T_a) )).

cnf(cls_xt1_I7_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_HOL_Oord__class_Oless(V_z,V_x,T_a)
    | ~ c_lessequals(V_z,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_x,T_a) )).

cnf(cls_xt1_I6_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | c_lessequals(V_z,V_x,T_a)
    | ~ c_lessequals(V_z,V_y,T_a)
    | ~ c_lessequals(V_y,V_x,T_a) )).

cnf(cls_pdivmod__rel__def_3,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | c_Polynomial_Opdivmod__rel(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_q,V_y,tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)),V_y,V_q,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_neg__0__less__iff__less_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_neg__0__less__iff__less_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_coeff__1_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Ocoeff(c_HOL_Oone__class_Oone(tc_Polynomial_Opoly(T_a)),c_HOL_Ozero__class_Ozero(tc_nat),T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_less__minus__self__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_less__minus__self__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_monom__eq__iff_0,axiom,
    ( ~ class_HOL_Ozero(T_a)
    | c_Polynomial_Omonom(V_a,V_n,T_a) != c_Polynomial_Omonom(V_b,V_n,T_a)
    | V_a = V_b )).

cnf(cls_order__less__asym_H_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_order__less__asym_0,axiom,
    ( ~ class_Orderings_Opreorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a) )).

cnf(cls_linorder__linear_0,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | c_lessequals(V_y,V_x,T_a)
    | c_lessequals(V_x,V_y,T_a) )).

cnf(cls_real__le__linear_0,axiom,
    ( c_lessequals(V_w,V_z,tc_RealDef_Oreal)
    | c_lessequals(V_z,V_w,tc_RealDef_Oreal) )).

cnf(cls_not__less__iff__gr__or__eq_1,axiom,
    ( ~ class_Orderings_Olinorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,V_y,T_a)
    | ~ c_HOL_Oord__class_Oless(V_y,V_x,T_a) )).

cnf(cls_xt1_I9_J_0,axiom,
    ( ~ class_Orderings_Oorder(T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a) )).

cnf(cls_Deriv_Oinverse__diff__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__less__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_x,T_a)
    | c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_x,T_a),c_HOL_Oone__class_Oone(T_a),T_a) )).

cnf(cls_one__le__inverse__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a)
    | ~ c_lessequals(c_HOL_Oone__class_Oone(T_a),c_HOL_Oinverse__class_Oinverse(V_x,T_a),T_a) )).

cnf(cls_inverse__less__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_x,T_a),c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_one__le__inverse__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oone__class_Oone(T_a),c_HOL_Oinverse__class_Oinverse(V_x,T_a),T_a)
    | ~ c_lessequals(V_x,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_kas_I2_J_0,axiom,
    ( v_k____ != c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_dvd__mult__cancel__left_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_dvd__mult__cancel__right_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_mult__less__cancel__left__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__less__cancel__left__neg_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__less__cancel__left__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mult__less__cancel__left__pos_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mult__strict__left__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a) )).

cnf(cls_mult__strict__left__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mult__less__cancel__left__disj_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mult__less__cancel__left__disj_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__strict__right__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a) )).

cnf(cls_mult__strict__right__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mult__less__cancel__right__disj_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mult__less__cancel__right__disj_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__strict__left__mono__comm_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__comm__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mult__divide__mult__cancel__left_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__divide__mult__cancel__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__neg__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__pos__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__less__mult__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__less__mult__pos2_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_b,V_a,T_a),T_a) )).

cnf(cls_div__mult__self1__is__id_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(V_b,V_a,T_a),V_b,T_a) = V_a
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__mult__self2__is__id_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_b,T_a) = V_a
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_divide__eq__eq_3,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_c,T_a) = V_a
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_eq__divide__eq_3,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | V_a = c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_c,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_divide__eq__imp_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_c,T_a) = V_a )).

cnf(cls_eq__divide__imp_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_c,T_a) )).

cnf(cls_dvd__mult__cancel__left_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a),c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a),T_a) )).

cnf(cls_dvd__mult__cancel__right_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a),c_HOL_Otimes__class_Otimes(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a),T_a) )).

cnf(cls_div__mult__mult1__if_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a),c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__le__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__le__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__le__0__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__le__0__iff_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__neg__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__pos__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_mult__pos__neg2_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_b,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_mult__mono1_0,axiom,
    ( ~ class_Ring__and__Field_Omult__mono1(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_mult__left__mono_0,axiom,
    ( ~ class_Ring__and__Field_Omult__mono(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_mult__right__mono_0,axiom,
    ( ~ class_Ring__and__Field_Omult__mono(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_mult__left__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,V_a,T_a) )).

cnf(cls_mult__right__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,V_a,T_a) )).

cnf(cls_split__mult__pos__le_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_split__mult__pos__le_1,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__nonpos__nonpos_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_zero__le__square_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_a,T_a),T_a) )).

cnf(cls_zero__le__mult__iff_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__le__mult__iff_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__nonneg__nonneg_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__cancel__semiring(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__le__mult__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__le__mult__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__le__mult__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_zero__le__mult__iff_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_mult__mono_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(V_c,V_d,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_mult__mono_H_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(V_c,V_d,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_div__mult__mult1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) = c_Divides_Odiv__class_Odiv(V_a,V_b,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__mult__mult2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) = c_Divides_Odiv__class_Odiv(V_a,V_b,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__less__cancel__left__disj_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_mod__by__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = V_a )).

cnf(cls_dvd__0__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_divide__self__if_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_neg__equal__zero_0,axiom,
    ( ~ class_OrderedGroup_Oordered__ab__group__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(V_a,T_a) != V_a
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_neg__equal__0__iff__equal_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(V_a,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__by__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_eq__iff__diff__eq__0_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_x,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_right__minus__eq_1,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_x,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_diff__0__right_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = V_a )).

cnf(cls_diff__self_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__ringb_Osubr0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_x,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mod__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_neg__0__equal__iff__equal_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ozero__class_Ozero(T_a) != c_HOL_Ouminus__class_Ouminus(V_a,T_a)
    | c_HOL_Ozero__class_Ozero(T_a) = V_a )).

cnf(cls_eq__iff__diff__eq__0_1,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = V_b )).

cnf(cls_right__minus__eq_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = V_b )).

cnf(cls_class__ringb_Osubr0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Ominus__class_Ominus(V_x,V_y,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_x = V_y )).

cnf(cls_divide__zero__left_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_divide__eq__eq_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_divide__zero_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_divide_Ozero_0,axiom,
    ( ~ class_RealVector_Oreal__normed__field(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__less__le__imp__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(V_c,V_d,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mult__le__less__imp__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,V_d,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_divide__strict__left__mono__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_c,V_a,T_a),c_HOL_Oinverse__class_Odivide(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_divide__strict__left__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_c,V_a,T_a),c_HOL_Oinverse__class_Odivide(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_a,T_a) )).

cnf(cls_less__divide__eq_7,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_less__divide__eq_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_less__divide__eq_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_less__divide__eq_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_divide__less__eq_7,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_divide__less__eq_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_divide__less__eq_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_divide__less__eq_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a) )).

cnf(cls_less__divide__eq_9,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_less__divide__eq_8,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_divide__less__eq_9,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_divide__less__eq_8,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_diff__frac__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Oinverse__class_Odivide(V_w,V_z,T_a),T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),c_HOL_Otimes__class_Otimes(V_w,V_y,T_a),T_a),c_HOL_Otimes__class_Otimes(V_y,V_z,T_a),T_a)
    | V_z = c_HOL_Ozero__class_Ozero(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__strict__mono_H_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,V_d,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mult__imp__less__div__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(V_z,c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_z,V_y,T_a),V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a) )).

cnf(cls_mult__imp__div__pos__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),V_z,T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,c_HOL_Otimes__class_Otimes(V_z,V_y,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a) )).

cnf(cls_less__divide__eq_6,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_less__divide__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_divide__less__eq_6,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_divide__less__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a) )).

cnf(cls_mult__less__imp__less__right_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_mult__less__imp__less__left_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_mult__right__less__imp__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_mult__left__less__imp__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_mult__strict__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,V_d,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mult__right__le__imp__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_mult__left__le__imp__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semiring__strict(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_mult__le__cancel__left__pos_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mult__le__cancel__left__pos_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_mult__le__cancel__left__neg_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__le__cancel__left__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(V_b,V_a,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_le__divide__eq_9,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_divide__le__eq_9,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),V_b,T_a)
    | ~ c_lessequals(V_b,c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),T_a) )).

cnf(cls_add__strict__increasing_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),T_a)
    | ~ c_lessequals(V_b,V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_add__pos__nonneg_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_add__nonneg__pos_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_le__eq__neg_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_le__eq__neg_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_add__nonpos__neg_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_add__neg__nonpos_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_add__strict__increasing2_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_c,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_mod__mult__mult2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Omod(V_a,V_b,T_a),V_c,T_a) )).

cnf(cls_mod__mult__mult1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_c,c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a) )).

cnf(cls_mult__dvd__mono_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_c,V_d,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_minus__mult__commute_0,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_mod__mult__cong_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_b,V_c,T_a) != c_Divides_Odiv__class_Omod(V_b_H,V_c,T_a)
    | c_Divides_Odiv__class_Omod(V_a,V_c,T_a) != c_Divides_Odiv__class_Omod(V_a_H,V_c,T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a_H,V_b_H,T_a),V_c,T_a) )).

cnf(cls_mod__mult__eq_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) )).

cnf(cls_dvd__mult__cancel__right_2,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__mult__cancel__left_2,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_mult__less__cancel__right__disj_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_mult__less__cancel__left__disj_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_minus__mult__minus_0,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_a,V_b,T_a) )).

cnf(cls_square__eq__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(V_b,T_a),c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_b,V_b,T_a) )).

cnf(cls_mult__right_Ominus_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_xa,c_HOL_Ouminus__class_Ouminus(V_x,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(V_xa,V_x,T_a),T_a) )).

cnf(cls_mult_Ominus__right_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_mult__left_Ominus_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(V_x,T_a),V_y,T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a) )).

cnf(cls_mult_Ominus__left_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_dvd__mult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_b,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) )).

cnf(cls_dvd__mult__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) )).

cnf(cls_dvd__mult2_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_dvd__mult_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_c,T_a) )).

cnf(cls_minus__mult__right_0,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_minus__mult__left_0,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a) )).

cnf(cls_mult__frac__frac_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Oinverse__class_Odivide(V_z,V_w,T_a),T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),c_HOL_Otimes__class_Otimes(V_y,V_w,T_a),T_a) )).

cnf(cls_square__eq__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,V_a,T_a) != c_HOL_Otimes__class_Otimes(V_b,V_b,T_a)
    | V_a = c_HOL_Ouminus__class_Ouminus(V_b,T_a)
    | V_a = V_b )).

cnf(cls_zmod__simps_I4_J_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) )).

cnf(cls_dvd__triv__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,c_HOL_Otimes__class_Otimes(V_b,V_a,T_a),T_a) )).

cnf(cls_dvd__triv__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) )).

cnf(cls_dvdI_0,axiom,
    ( ~ class_Ring__and__Field_Odvd(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_b,c_HOL_Otimes__class_Otimes(V_b,V_k,T_a),T_a) )).

cnf(cls_mult__right_Odiff_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_xa,c_HOL_Ominus__class_Ominus(V_x,V_y,T_a),T_a) = c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_xa,V_x,T_a),c_HOL_Otimes__class_Otimes(V_xa,V_y,T_a),T_a) )).

cnf(cls_mult_Odiff__right_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ominus__class_Ominus(V_b,V_b_H,T_a),T_a) = c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Otimes__class_Otimes(V_a,V_b_H,T_a),T_a) )).

cnf(cls_mult_Odiff__left_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_a_H,T_a),V_b,T_a) = c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Otimes__class_Otimes(V_a_H,V_b,T_a),T_a) )).

cnf(cls_mult__left_Odiff_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_x,V_y,T_a),V_ya,T_a) = c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_x,V_ya,T_a),c_HOL_Otimes__class_Otimes(V_y,V_ya,T_a),T_a) )).

cnf(cls_mod__mult__right__eq_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) )).

cnf(cls_mod__mult__left__eq_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),V_b,T_a),V_c,T_a) )).

cnf(cls_mult__less__cancel__right__disj_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_not__square__less__zero_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__le__0__iff_4,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_mult__le__0__iff_5,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_split__mult__neg__le_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__cancel__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_split__mult__neg__le_1,axiom,
    ( ~ class_Ring__and__Field_Opordered__cancel__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__nonneg__nonpos_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__cancel__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_mult__nonpos__nonneg_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__cancel__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__nonneg__nonpos2_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__cancel__semiring(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_b,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_eq__divide__eq_4,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Ozero__class_Ozero(T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a),V_c,T_a) )).

cnf(cls_divide__eq__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | V_b = c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_c,T_a)
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_eq__divide__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),V_c,T_a) = V_b
    | V_c = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mod__mult__self1__is__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_b,V_a,T_a),V_b,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mod__mult__self2__is__0_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_b,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__less__cancel__left__disj_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_mult__less__cancel__left__disj_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_c,V_a,T_a),c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a) )).

cnf(cls_mult__less__cancel__right__disj_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_mult__less__cancel__right__disj_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_frac__eq__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a) != c_HOL_Oinverse__class_Odivide(V_w,V_z,T_a)
    | V_z = c_HOL_Ozero__class_Ozero(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,V_z,T_a) = c_HOL_Otimes__class_Otimes(V_w,V_y,T_a) )).

cnf(cls_frac__eq__eq_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,V_z,T_a) != c_HOL_Otimes__class_Otimes(V_w,V_y,T_a)
    | V_z = c_HOL_Ozero__class_Ozero(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a)
    | c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a) = c_HOL_Oinverse__class_Odivide(V_w,V_z,T_a) )).

cnf(cls_divide__eq__eq_4,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a),V_c,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_add__neg__neg_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_left__minus_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_right__minus_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_ab__left__minus_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_eq__neg__iff__add__eq__0_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_b,T_a),V_b,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_add__nonneg__eq__0__iff_2,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_add__increasing_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_lessequals(V_b,c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),T_a)
    | ~ c_lessequals(V_b,V_c,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_add__increasing2_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_lessequals(V_b,c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),T_a)
    | ~ c_lessequals(V_b,V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_c,T_a) )).

cnf(cls_add__nonneg__eq__0__iff_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,V_y,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a)
    | V_x = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_add__nonneg__eq__0__iff_1,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,V_y,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_even__less__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_even__less__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_double__add__less__zero__iff__single__less__zero_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_double__add__less__zero__iff__single__less__zero_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_pos__add__strict_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,V_c,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__less__double__add__iff__zero__less__single__add_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),T_a) )).

cnf(cls_zero__less__double__add__iff__zero__less__single__add_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_add__nonneg__nonneg_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__le__double__add__iff__zero__le__single__add_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),T_a) )).

cnf(cls_zero__le__double__add__iff__zero__le__single__add_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_minus__unique_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | c_HOL_Ouminus__class_Ouminus(V_a,T_a) = V_b )).

cnf(cls_eq__neg__iff__add__eq__0_1,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ouminus__class_Ouminus(V_b,T_a) )).

cnf(cls_add__pos__pos_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_double__add__le__zero__iff__single__add__le__zero_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_double__add__le__zero__iff__single__add__le__zero_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_add__nonpos__nonpos_0,axiom,
    ( ~ class_OrderedGroup_Opordered__comm__monoid__add(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__self__if_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(V_a,V_a,T_a) = c_HOL_Oone__class_Oone(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_divide__self_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(V_a,V_a,T_a) = c_HOL_Oone__class_Oone(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_right__inverse__eq_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | V_x = c_HOL_Ozero__class_Ozero(T_a)
    | c_HOL_Oinverse__class_Odivide(V_x,V_x,T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_right__inverse__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a) != c_HOL_Oone__class_Oone(T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = V_b )).

cnf(cls_div__self_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(V_a,V_a,T_a) = c_HOL_Oone__class_Oone(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_not__one__less__zero_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_zero__le__one_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oone__class_Oone(T_a),T_a) )).

cnf(cls_mod__by__1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_a,c_HOL_Oone__class_Oone(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_not__one__le__zero_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | ~ c_lessequals(c_HOL_Oone__class_Oone(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_zero__less__one_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oone__class_Oone(T_a),T_a) )).

cnf(cls_mod__mult__self1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(V_a,V_b,T_a) )).

cnf(cls_mod__mult__self2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(V_a,V_b,T_a) )).

cnf(cls_mult_Oprod__diff__prod_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_x,V_a,T_a),c_HOL_Ominus__class_Ominus(V_y,V_b,T_a),T_a),c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_x,V_a,T_a),V_b,T_a),T_a),c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ominus__class_Ominus(V_y,V_b,T_a),T_a),T_a) )).

cnf(cls_eq__add__iff2_1,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),V_e,T_a),V_d,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a) )).

cnf(cls_eq__add__iff1_1,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_e,T_a),V_c,T_a),T_a) )).

cnf(cls_eq__add__iff1_0,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a) != c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_e,T_a),V_c,T_a) = V_d )).

cnf(cls_eq__add__iff2_0,axiom,
    ( ~ class_Ring__and__Field_Oring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a) != c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a)
    | V_c = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),V_e,T_a),V_d,T_a) )).

cnf(cls_less__1__mult_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),c_HOL_Otimes__class_Otimes(V_m,V_n,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_n,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_m,T_a) )).

cnf(cls_class__ring_Oneg__mul_0,axiom,
    ( ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Ouminus__class_Ouminus(V_x,T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Ouminus__class_Ouminus(c_HOL_Oone__class_Oone(T_a),T_a),V_x,T_a) )).

cnf(cls_power__le__zero__eq_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | V_x = c_HOL_Ozero__class_Ozero(T_a)
    | c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_Power_Opower__class_Opower(V_x,V_n,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_power__less__zero__eq_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_Power_Opower__class_Opower(V_x,V_n,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_power__mono_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_lessequals(c_Power_Opower__class_Opower(V_a,V_n,T_a),c_Power_Opower__class_Opower(V_b,V_n,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_power__eq__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ozero__neq__one(T_a)
    | ~ class_Ring__and__Field_Ono__zero__divisors(T_a)
    | ~ class_Ring__and__Field_Omult__zero(T_a)
    | ~ class_Power_Opower(T_a)
    | c_Power_Opower__class_Opower(V_a,c_HOL_Ozero__class_Ozero(tc_nat),T_a) != c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_power__eq__0__iff_2,axiom,
    ( ~ class_Ring__and__Field_Ozero__neq__one(T_a)
    | ~ class_Ring__and__Field_Ono__zero__divisors(T_a)
    | ~ class_Ring__and__Field_Omult__zero(T_a)
    | ~ class_Power_Opower(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Ozero__class_Ozero(T_a),V_n,T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | V_n = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_power__0__left_1,axiom,
    ( ~ class_Ring__and__Field_Osemiring__0(T_a)
    | ~ class_Power_Opower(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Ozero__class_Ozero(T_a),V_n,T_a) = c_HOL_Ozero__class_Ozero(T_a)
    | V_n = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_zero__le__power__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__le__power_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_zero__less__power_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_nonzero__power__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),V_n,T_a) = c_HOL_Oinverse__class_Odivide(c_Power_Opower__class_Opower(V_a,V_n,T_a),c_Power_Opower__class_Opower(V_b,V_n,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_less__add__one_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oone__class_Oone(T_a),T_a),T_a) )).

cnf(cls_power__add_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__mult(T_a)
    | c_Power_Opower__class_Opower(V_a,c_HOL_Oplus__class_Oplus(V_m,V_n,tc_nat),T_a) = c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_a,V_m,T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a) )).

cnf(cls_class__semiring_Omul__pwr_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_x,V_p,T_a),c_Power_Opower__class_Opower(V_x,V_q,T_a),T_a) = c_Power_Opower__class_Opower(V_x,c_HOL_Oplus__class_Oplus(V_p,V_q,tc_nat),T_a) )).

cnf(cls_power__one__over_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a) = c_Power_Opower__class_Opower(c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_a,T_a),V_n,T_a) )).

cnf(cls_dvd__power_1,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Oone__class_Oone(T_a),c_Power_Opower__class_Opower(c_HOL_Oone__class_Oone(T_a),V_n,T_a),T_a) )).

cnf(cls_power__0_0,axiom,
    ( ~ class_Power_Opower(T_a)
    | c_Power_Opower__class_Opower(V_a,c_HOL_Ozero__class_Ozero(tc_nat),T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_class__semiring_Opwr__0_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Power_Opower__class_Opower(V_x,c_HOL_Ozero__class_Ozero(tc_nat),T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_one__le__power_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_lessequals(c_HOL_Oone__class_Oone(T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a)
    | ~ c_lessequals(c_HOL_Oone__class_Oone(T_a),V_a,T_a) )).

cnf(cls_power__inject__exp_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_Power_Opower__class_Opower(V_a,V_m,T_a) != c_Power_Opower__class_Opower(V_a,V_n,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_a,T_a)
    | V_m = V_n )).

cnf(cls_inverse__positive__iff__positive_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) )).

cnf(cls_inverse__positive__iff__positive_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_positive__imp__inverse__positive_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_inverse__nonpositive__iff__nonpositive_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_inverse__nonpositive__iff__nonpositive_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_inverse__negative__imp__negative_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_inverse__positive__imp__positive_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) )).

cnf(cls_inverse__less__imp__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) )).

cnf(cls_less__imp__inverse__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_b,T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_inverse__less__imp__less__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) )).

cnf(cls_less__imp__inverse__less__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_b,T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_inverse__nonnegative__iff__nonnegative_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) )).

cnf(cls_inverse__nonnegative__iff__nonnegative_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_nonzero__inverse__minus__eq_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__negative__iff__negative_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_mult__frac__num_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),V_z,T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),V_y,T_a) )).

cnf(cls_zero__less__divide__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a) )).

cnf(cls_zero__less__divide__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),T_a) )).

cnf(cls_divide__less__0__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__less__0__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__le__0__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_divide__le__0__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_zero__le__divide__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a) )).

cnf(cls_zero__le__divide__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_b,T_a),T_a) )).

cnf(cls_semiring__div__class_Omod__div__equality_H_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Omod(V_a,V_b,T_a),c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),V_b,T_a),T_a) = V_a )).

cnf(cls_less__add__iff2_1,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),V_e,T_a),V_d,T_a),T_a) )).

cnf(cls_less__add__iff2_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_HOL_Oord__class_Oless(V_c,c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),V_e,T_a),V_d,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a) )).

cnf(cls_less__add__iff1_1,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_e,T_a),V_c,T_a),V_d,T_a) )).

cnf(cls_less__add__iff1_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_e,T_a),V_c,T_a),V_d,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a) )).

cnf(cls_div__mod__equality2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),T_a),c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(V_a,V_c,T_a) )).

cnf(cls_div__mod__equality_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),V_b,T_a),c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(V_a,V_c,T_a) )).

cnf(cls_DERIV__mult__lemma_0,axiom,
    ( ~ class_RealVector_Oreal__field(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Otimes__class_Otimes(V_c,V_d,T_a),T_a),V_h,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(V_b,V_d,T_a),V_h,T_a),T_a),c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(V_a,V_c,T_a),V_h,T_a),V_d,T_a),T_a) )).

cnf(cls_mod__div__equality2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),T_a),c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a) = V_a )).

cnf(cls_mod__div__equality_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),V_b,T_a),c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a) = V_a )).

cnf(cls_mod__div__decomp_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | V_a = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),V_b,T_a),c_Divides_Odiv__class_Omod(V_a,V_b,T_a),T_a) )).

cnf(cls_inf__period_I4_J_1,axiom,
    ( ~ class_Ring__and__Field_Odvd(T_a)
    | ~ class_Ring__and__Field_Ocomm__ring(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_d,c_HOL_Oplus__class_Oplus(V_x,V_t,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_d,c_HOL_Oplus__class_Oplus(c_HOL_Ominus__class_Ominus(V_x,c_HOL_Otimes__class_Otimes(V_k,V_D,T_a),T_a),V_t,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_d,V_D,T_a) )).

cnf(cls_inf__period_I4_J_0,axiom,
    ( ~ class_Ring__and__Field_Odvd(T_a)
    | ~ class_Ring__and__Field_Ocomm__ring(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_d,c_HOL_Oplus__class_Oplus(c_HOL_Ominus__class_Ominus(V_x,c_HOL_Otimes__class_Otimes(V_k,V_D,T_a),T_a),V_t,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_d,c_HOL_Oplus__class_Oplus(V_x,V_t,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_d,V_D,T_a) )).

cnf(cls_le__add__iff2_1,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a)
    | ~ c_lessequals(V_c,c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),V_e,T_a),V_d,T_a),T_a) )).

cnf(cls_le__add__iff2_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(V_c,c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),V_e,T_a),V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a) )).

cnf(cls_le__add__iff1_1,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_e,T_a),V_c,T_a),V_d,T_a) )).

cnf(cls_le__add__iff1_0,axiom,
    ( ~ class_Ring__and__Field_Opordered__ring(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_e,T_a),V_c,T_a),V_d,T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),V_c,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_d,T_a),T_a) )).

cnf(cls_minus__add__distrib_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Ouminus__class_Ouminus(V_b,T_a),T_a) )).

cnf(cls_minus__add_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ouminus__class_Ouminus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_b,T_a),c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) )).

cnf(cls_le__add__right__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__group__add(T_a)
    | c_lessequals(V_a,c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(V_c,V_d,T_a)
    | ~ c_lessequals(V_a,c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) )).

cnf(cls_add__strict__right__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__cancel__ab__semigroup__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_add__less__cancel__right_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_add__less__cancel__right_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) )).

cnf(cls_add__strict__left__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__cancel__ab__semigroup__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_c,V_a,T_a),c_HOL_Oplus__class_Oplus(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_add__less__cancel__left_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_c,V_a,T_a),c_HOL_Oplus__class_Oplus(V_c,V_b,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_add__less__cancel__left_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_c,V_a,T_a),c_HOL_Oplus__class_Oplus(V_c,V_b,T_a),T_a) )).

cnf(cls_minus__add__cancel_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a) = V_b )).

cnf(cls_add__cancel__end_1,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_z,T_a),c_HOL_Oplus__class_Oplus(V_y,V_z,T_a),T_a) = V_y )).

cnf(cls_add__strict__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__cancel__ab__semigroup__add(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_c,V_d,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_mod__add__cong_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(V_b,V_c,T_a) != c_Divides_Odiv__class_Omod(V_b_H,V_c,T_a)
    | c_Divides_Odiv__class_Omod(V_a,V_c,T_a) != c_Divides_Odiv__class_Omod(V_a_H,V_c,T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a_H,V_b_H,T_a),V_c,T_a) )).

cnf(cls_add__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a)
    | ~ c_lessequals(V_c,V_d,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_dvd__add_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_a,c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_c,T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_a,V_b,T_a) )).

cnf(cls_zmod__simps_I1_J_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) )).

cnf(cls_zmod__simps_I2_J_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) )).

cnf(cls_add__minus__cancel_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_b,T_a),T_a) = V_b )).

cnf(cls_mod__add__self2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(V_a,V_b,T_a) )).

cnf(cls_mod__add__self1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_b,V_a,T_a),V_b,T_a) = c_Divides_Odiv__class_Omod(V_a,V_b,T_a) )).

cnf(cls_add__le__cancel__right_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_add__le__cancel__right_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) )).

cnf(cls_add__le__cancel__left_1,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_c,V_a,T_a),c_HOL_Oplus__class_Oplus(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_add__le__cancel__left_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(T_a)
    | c_lessequals(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(V_c,V_a,T_a),c_HOL_Oplus__class_Oplus(V_c,V_b,T_a),T_a) )).

cnf(cls_add__right__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_add__left__mono_0,axiom,
    ( ~ class_OrderedGroup_Opordered__ab__semigroup__add(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(V_c,V_a,T_a),c_HOL_Oplus__class_Oplus(V_c,V_b,T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_diff__add__cancel_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_b,T_a) = V_a )).

cnf(cls_mod__add__eq_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) )).

cnf(cls_mod__add__right__eq_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,c_Divides_Odiv__class_Omod(V_b,V_c,T_a),T_a),V_c,T_a) )).

cnf(cls_mod__add__left__eq_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_Divides_Odiv__class_Omod(c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Omod(V_a,V_c,T_a),V_b,T_a),V_c,T_a) )).

cnf(cls_Deriv_Oadd__diff__add_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),c_HOL_Ominus__class_Ominus(V_c,V_d,T_a),T_a) )).

cnf(cls_add__cancel__end_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,c_HOL_Oplus__class_Oplus(V_y,V_z,T_a),T_a) != V_y
    | V_x = c_HOL_Ouminus__class_Ouminus(V_z,T_a) )).

cnf(cls_add__diff__cancel_0,axiom,
    ( ~ class_OrderedGroup_Ogroup__add(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_b,T_a) = V_a )).

cnf(cls_add__divide__distrib_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oinverse__class_Odivide(V_a,V_c,T_a),c_HOL_Oinverse__class_Odivide(V_b,V_c,T_a),T_a) )).

cnf(cls_divide_Oadd_0,axiom,
    ( ~ class_RealVector_Oreal__normed__field(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(V_x,V_y,T_a),V_ya,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oinverse__class_Odivide(V_x,V_ya,T_a),c_HOL_Oinverse__class_Odivide(V_y,V_ya,T_a),T_a) )).

cnf(cls_power__le__zero__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | ~ c_lessequals(c_Power_Opower__class_Opower(V_x,c_HOL_Ozero__class_Ozero(tc_nat),T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_zero__less__power__eq_3,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_Power_Opower__class_Opower(V_x,c_HOL_Ozero__class_Ozero(tc_nat),T_a),T_a) )).

cnf(cls_power__less__imp__less__base_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(V_a,V_b,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(c_Power_Opower__class_Opower(V_a,V_n,T_a),c_Power_Opower__class_Opower(V_b,V_n,T_a),T_a) )).

cnf(cls_zero__less__power__eq_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | V_n = c_HOL_Ozero__class_Ozero(tc_nat)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_Power_Opower__class_Opower(c_HOL_Ozero__class_Ozero(T_a),V_n,T_a),T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_less__half__sum_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(V_a,c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Oone__class_Oone(T_a),c_HOL_Oone__class_Oone(T_a),T_a),T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_gt__half__sum_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Oone__class_Oone(T_a),c_HOL_Oone__class_Oone(T_a),T_a),T_a),V_b,T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,V_b,T_a) )).

cnf(cls_divide__1_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(V_a,c_HOL_Oone__class_Oone(T_a),T_a) = V_a )).

cnf(cls_one__dvd_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_HOL_Oone__class_Oone(T_a),V_a,T_a) )).

cnf(cls_div__by__1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(V_a,c_HOL_Oone__class_Oone(T_a),T_a) = V_a )).

cnf(cls_DERIV__inverse__lemma_0,axiom,
    ( ~ class_RealVector_Oreal__normed__field(T_a)
    | c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a),V_h,T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Odivide(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),V_h,T_a),T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_le__imp__inverse__le__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Oinverse(V_b,T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_inverse__le__imp__le__neg_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(V_b,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) )).

cnf(cls_le__imp__inverse__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Oinverse(V_b,T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(V_a,V_b,T_a) )).

cnf(cls_inverse__le__imp__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_lessequals(V_b,V_a,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) )).

cnf(cls_power__one__right_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__mult(T_a)
    | c_Power_Opower__class_Opower(V_a,c_HOL_Oone__class_Oone(tc_nat),T_a) = V_a )).

cnf(cls_class__semiring_Osemiring__rules_I33_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Power_Opower__class_Opower(V_x,c_HOL_Oone__class_Oone(tc_nat),T_a) = V_x )).

cnf(cls_power__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),V_n,T_a) = c_HOL_Oinverse__class_Odivide(c_Power_Opower__class_Opower(V_a,V_n,T_a),c_Power_Opower__class_Opower(V_b,V_n,T_a),T_a) )).

cnf(cls_dvd__power__same_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Power_Opower__class_Opower(V_x,V_n,T_a),c_Power_Opower__class_Opower(V_y,V_n,T_a),T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_x,V_y,T_a) )).

cnf(cls_norm__mult__less_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a),c_HOL_Otimes__class_Otimes(V_r,V_s,tc_RealDef_Oreal),tc_RealDef_Oreal)
    | ~ c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(V_y,T_a),V_s,tc_RealDef_Oreal)
    | ~ c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(V_x,T_a),V_r,tc_RealDef_Oreal) )).

cnf(cls_norm__mult__ineq_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_lessequals(c_RealVector_Onorm__class_Onorm(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a),c_HOL_Otimes__class_Otimes(c_RealVector_Onorm__class_Onorm(V_x,T_a),c_RealVector_Onorm__class_Onorm(V_y,T_a),tc_RealDef_Oreal),tc_RealDef_Oreal) )).

cnf(cls_norm__power__ineq_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra__1(T_a)
    | c_lessequals(c_RealVector_Onorm__class_Onorm(c_Power_Opower__class_Opower(V_x,V_n,T_a),T_a),c_Power_Opower__class_Opower(c_RealVector_Onorm__class_Onorm(V_x,T_a),V_n,tc_RealDef_Oreal),tc_RealDef_Oreal) )).

cnf(cls_smult__dvd__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(c_HOL_Ozero__class_Ozero(T_a),V_p,T_a),V_q,tc_Polynomial_Opoly(T_a))
    | V_q = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_smult__dvd__iff_3,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(c_HOL_Ozero__class_Ozero(T_a),V_p,T_a),c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_degree__smult__eq_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Odegree(c_Polynomial_Osmult(c_HOL_Ozero__class_Ozero(T_a),V_p,T_a),T_a) = c_HOL_Ozero__class_Ozero(tc_nat) )).

cnf(cls_norm__minus__cancel_0,axiom,
    ( ~ class_RealVector_Oreal__normed__vector(T_a)
    | c_RealVector_Onorm__class_Onorm(c_HOL_Ouminus__class_Ouminus(V_x,T_a),T_a) = c_RealVector_Onorm__class_Onorm(V_x,T_a) )).

cnf(cls_norm__minus__commute_0,axiom,
    ( ~ class_RealVector_Oreal__normed__vector(T_a)
    | c_RealVector_Onorm__class_Onorm(c_HOL_Ominus__class_Ominus(V_a,V_b,T_a),T_a) = c_RealVector_Onorm__class_Onorm(c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),T_a) )).

cnf(cls_inverse__minus__eq_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Ouminus__class_Ouminus(V_a,T_a),T_a) = c_HOL_Ouminus__class_Ouminus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) )).

cnf(cls_inverse__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a),T_a) = c_HOL_Oinverse__class_Odivide(V_b,V_a,T_a) )).

cnf(cls_synthetic__div__unique_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Oplus__class_Oplus(V_p,c_Polynomial_Osmult(V_c,V_q,T_a),tc_Polynomial_Opoly(T_a)) != c_Polynomial_OpCons(V_r,V_q,T_a)
    | V_r = c_Polynomial_Opoly(V_p,V_c,T_a) )).

cnf(cls_smult__0__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(V_a,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_smult__eq__0__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Osmult(V_a,c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_mod__smult__left_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Omod(c_Polynomial_Osmult(V_a,V_x,T_a),V_y,tc_Polynomial_Opoly(T_a)) = c_Polynomial_Osmult(V_a,c_Divides_Odiv__class_Omod(V_x,V_y,tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_pdivmod__rel__smult__left_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(c_Polynomial_Osmult(V_a,V_x,T_a),V_y,c_Polynomial_Osmult(V_a,V_q,T_a),c_Polynomial_Osmult(V_a,V_r,T_a),T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q,V_r,T_a) )).

cnf(cls_smult__add__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(V_a,c_HOL_Oplus__class_Oplus(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a) = c_HOL_Oplus__class_Oplus(c_Polynomial_Osmult(V_a,V_p,T_a),c_Polynomial_Osmult(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_div__smult__left_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Odiv(c_Polynomial_Osmult(V_a,V_x,T_a),V_y,tc_Polynomial_Opoly(T_a)) = c_Polynomial_Osmult(V_a,c_Divides_Odiv__class_Odiv(V_x,V_y,tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_smult__dvd__cancel_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(V_a,V_p,T_a),V_q,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_dvd__smult_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_p,c_Polynomial_Osmult(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_mult__smult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(V_p,c_Polynomial_Osmult(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a)) = c_Polynomial_Osmult(V_a,c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_mult__smult__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_HOL_Otimes__class_Otimes(c_Polynomial_Osmult(V_a,V_p,T_a),V_q,tc_Polynomial_Opoly(T_a)) = c_Polynomial_Osmult(V_a,c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),T_a) )).

cnf(cls_inverse__negative__iff__negative_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_negative__imp__inverse__negative_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_norm__mult_0,axiom,
    ( ~ class_RealVector_Oreal__normed__div__algebra(T_a)
    | c_RealVector_Onorm__class_Onorm(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_RealVector_Onorm__class_Onorm(V_x,T_a),c_RealVector_Onorm__class_Onorm(V_y,T_a),tc_RealDef_Oreal) )).

cnf(cls_divide__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Odivide(V_a,V_b,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) )).

cnf(cls_class__fieldgb_Odivide__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a) = c_HOL_Otimes__class_Otimes(V_x,c_HOL_Oinverse__class_Oinverse(V_y,T_a),T_a) )).

cnf(cls_poly__replicate__append_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__ring__1(T_a)
    | c_Polynomial_Opoly(c_HOL_Otimes__class_Otimes(c_Polynomial_Omonom(c_HOL_Oone__class_Oone(T_a),V_n,T_a),V_p,tc_Polynomial_Opoly(T_a)),V_x,T_a) = c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_x,V_n,T_a),c_Polynomial_Opoly(V_p,V_x,T_a),T_a) )).

cnf(cls_norm__one_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra__1(T_a)
    | c_RealVector_Onorm__class_Onorm(c_HOL_Oone__class_Oone(T_a),T_a) = c_HOL_Oone__class_Oone(tc_RealDef_Oreal) )).

cnf(cls_class__fieldgb_Oinverse__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_x,T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_x,T_a) )).

cnf(cls_inverse__eq__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_a,T_a) )).

cnf(cls_one__less__inverse__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(V_x,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),c_HOL_Oinverse__class_Oinverse(V_x,T_a),T_a) )).

cnf(cls_inverse__le__1__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Oinverse(V_x,T_a),c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oone__class_Oone(T_a),V_x,T_a) )).

cnf(cls_one__le__inverse__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(V_x,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oone__class_Oone(T_a),c_HOL_Oinverse__class_Oinverse(V_x,T_a),T_a) )).

cnf(cls_inverse__less__1__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oinverse__class_Oinverse(V_x,T_a),c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_x,T_a) )).

cnf(cls_kas_I3_J_0,axiom,
    ( c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(v_s____,tc_Complex_Ocomplex),v_k____,tc_nat),c_HOL_Oone__class_Oone(tc_nat),tc_nat) = c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_norm__power_0,axiom,
    ( ~ class_RealVector_Oreal__normed__div__algebra(T_a)
    | c_RealVector_Onorm__class_Onorm(c_Power_Opower__class_Opower(V_x,V_n,T_a),T_a) = c_Power_Opower__class_Opower(c_RealVector_Onorm__class_Onorm(V_x,T_a),V_n,tc_RealDef_Oreal) )).

cnf(cls_poly__decompose__lemma_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(t_a)
    | v_sko__Fundamental__Theorem__Algebra__Mirabelle__Xpoly__decompose__lemma__2(V_p) != c_HOL_Ozero__class_Ozero(t_a)
    | c_Polynomial_Opoly(V_p,V_z,t_a) = c_HOL_Ozero__class_Ozero(t_a)
    | V_z = c_HOL_Ozero__class_Ozero(t_a) )).

cnf(cls_order__root_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Opoly(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_poly__0_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Opoly(c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)),V_x,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_degree__smult__eq_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Odegree(c_Polynomial_Osmult(V_a,V_p,T_a),T_a) = c_Polynomial_Odegree(V_p,T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_smult__dvd__iff_2,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(V_a,V_p,T_a),V_q,tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_smult__dvd__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(V_a,V_p,T_a),V_q,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_dvd__smult__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_p,c_Polynomial_Osmult(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_dvd__smult__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a))
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,c_Polynomial_Osmult(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_smult__dvd_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(c_Polynomial_Osmult(V_a,V_p,T_a),V_q,tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a)) )).

cnf(cls_dvd__smult__cancel_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Ring__and__Field_Odvd__class_Odvd(V_p,V_q,tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_Ring__and__Field_Odvd__class_Odvd(V_p,c_Polynomial_Osmult(V_a,V_q,T_a),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_smult__eq__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Osmult(V_a,V_p,T_a) != c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_p = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mod__smult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Omod(V_x,c_Polynomial_Osmult(V_a,V_y,T_a),tc_Polynomial_Opoly(T_a)) = c_Divides_Odiv__class_Omod(V_x,V_y,tc_Polynomial_Opoly(T_a))
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_smult__eq__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | c_Polynomial_Osmult(c_HOL_Ozero__class_Ozero(T_a),V_p,T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_smult__0__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(c_HOL_Ozero__class_Ozero(T_a),V_p,T_a) = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(T_a)) )).

cnf(cls_poly__mult_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Opoly(c_HOL_Otimes__class_Otimes(V_p,V_q,tc_Polynomial_Opoly(T_a)),V_x,T_a) = c_HOL_Otimes__class_Otimes(c_Polynomial_Opoly(V_p,V_x,T_a),c_Polynomial_Opoly(V_q,V_x,T_a),T_a) )).

cnf(cls_poly__offset__poly_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Opoly(c_Fundamental__Theorem__Algebra__Mirabelle_Ooffset__poly(V_p,V_h,T_a),V_x,T_a) = c_Polynomial_Opoly(V_p,c_HOL_Oplus__class_Oplus(V_h,V_x,T_a),T_a) )).

cnf(cls_poly__add_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Opoly(c_HOL_Oplus__class_Oplus(V_p,V_q,tc_Polynomial_Opoly(T_a)),V_x,T_a) = c_HOL_Oplus__class_Oplus(c_Polynomial_Opoly(V_p,V_x,T_a),c_Polynomial_Opoly(V_q,V_x,T_a),T_a) )).

cnf(cls_coeff__smult_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Ocoeff(c_Polynomial_Osmult(V_a,V_p,T_a),V_n,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_Polynomial_Ocoeff(V_p,V_n,T_a),T_a) )).

cnf(cls_smult__pCons_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(V_a,c_Polynomial_OpCons(V_b,V_p,T_a),T_a) = c_Polynomial_OpCons(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_Polynomial_Osmult(V_a,V_p,T_a),T_a) )).

cnf(cls_smult__monom_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(V_a,c_Polynomial_Omonom(V_b,V_n,T_a),T_a) = c_Polynomial_Omonom(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_n,T_a) )).

cnf(cls_smult__add__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_p,T_a) = c_HOL_Oplus__class_Oplus(c_Polynomial_Osmult(V_a,V_p,T_a),c_Polynomial_Osmult(V_b,V_p,T_a),tc_Polynomial_Opoly(T_a)) )).

cnf(cls_poly__1_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Opoly(c_HOL_Oone__class_Oone(tc_Polynomial_Opoly(T_a)),V_x,T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_norm__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | ~ class_RealVector_Oreal__normed__div__algebra(T_a)
    | c_RealVector_Onorm__class_Onorm(c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) = c_HOL_Oinverse__class_Oinverse(c_RealVector_Onorm__class_Onorm(V_a,T_a),tc_RealDef_Oreal) )).

cnf(cls_poly__power_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Opoly(c_Power_Opower__class_Opower(V_p,V_n,tc_Polynomial_Opoly(T_a)),V_x,T_a) = c_Power_Opower__class_Opower(c_Polynomial_Opoly(V_p,V_x,T_a),V_n,T_a) )).

cnf(cls_cq0_0,axiom,
    ( c_lessequals(c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(v_pa____,V_w,tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_RealDef_Oreal) )).

cnf(cls_expi__add_0,axiom,
    ( c_Complex_Oexpi(c_HOL_Oplus__class_Oplus(V_a,V_b,tc_Complex_Ocomplex)) = c_HOL_Otimes__class_Otimes(c_Complex_Oexpi(V_a),c_Complex_Oexpi(V_b),tc_Complex_Ocomplex) )).

cnf(cls_poly__offset_1,axiom,
    ( c_Polynomial_Opoly(v_sko__Fundamental__Theorem__Algebra__Mirabelle__Xpoly__offset__1(V_a,V_p),V_q,tc_Complex_Ocomplex) = c_Polynomial_Opoly(V_p,c_HOL_Oplus__class_Oplus(V_a,V_q,tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_complex__divide__def_0,axiom,
    ( c_HOL_Oinverse__class_Odivide(V_x,V_y,tc_Complex_Ocomplex) = c_HOL_Otimes__class_Otimes(V_x,c_HOL_Oinverse__class_Oinverse(V_y,tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_expi__zero_0,axiom,
    ( c_Complex_Oexpi(c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex)) = c_HOL_Oone__class_Oone(tc_Complex_Ocomplex) )).

cnf(cls_sum__squares__gt__zero__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a),T_a)
    | V_x = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_sum__squares__gt__zero__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a),T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_sum__squares__le__zero__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | V_x = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_sum__squares__le__zero__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a)
    | ~ c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_sum__squares__ge__zero_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a),T_a) )).

cnf(cls_not__sum__squares__lt__zero_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_sum__squares__le__zero__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_lessequals(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a),c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a),T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_sum__squares__gt__zero__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a),c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a),T_a),T_a) )).

cnf(cls_add__frac__num_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),V_z,T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(V_x,c_HOL_Otimes__class_Otimes(V_z,V_y,T_a),T_a),V_y,T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_add__num__frac_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oplus__class_Oplus(V_z,c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(V_x,c_HOL_Otimes__class_Otimes(V_z,V_y,T_a),T_a),V_y,T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__mult__self1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Oplus__class_Oplus(V_a,c_HOL_Otimes__class_Otimes(V_c,V_b,T_a),T_a),V_b,T_a) = c_HOL_Oplus__class_Oplus(V_c,c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__mult__self2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Oplus__class_Oplus(V_a,c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a),V_b,T_a) = c_HOL_Oplus__class_Oplus(V_c,c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_add__frac__eq_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oinverse__class_Odivide(V_x,V_y,T_a),c_HOL_Oinverse__class_Odivide(V_w,V_z,T_a),T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),c_HOL_Otimes__class_Otimes(V_w,V_y,T_a),T_a),c_HOL_Otimes__class_Otimes(V_y,V_z,T_a),T_a)
    | V_z = c_HOL_Ozero__class_Ozero(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__left__le__one__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_y,V_x,T_a),V_x,T_a)
    | ~ c_lessequals(V_y,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_mult__right__le__one__le_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__idom(T_a)
    | c_lessequals(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),V_x,T_a)
    | ~ c_lessequals(V_y,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a)
    | ~ c_lessequals(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_div__add__self1_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Oplus__class_Oplus(V_b,V_a,T_a),V_b,T_a) = c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),c_HOL_Oone__class_Oone(T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__add__self2_0,axiom,
    ( ~ class_Divides_Osemiring__div(T_a)
    | c_Divides_Odiv__class_Odiv(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_b,T_a) = c_HOL_Oplus__class_Oplus(c_Divides_Odiv__class_Odiv(V_a,V_b,T_a),c_HOL_Oone__class_Oone(T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_zero__less__two_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Oplus__class_Oplus(c_HOL_Oone__class_Oone(T_a),c_HOL_Oone__class_Oone(T_a),T_a),T_a) )).

cnf(cls_power__0__left_0,axiom,
    ( ~ class_Ring__and__Field_Osemiring__0(T_a)
    | ~ class_Power_Opower(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(tc_nat),T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_power__gt1__lemma_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),c_HOL_Otimes__class_Otimes(V_a,c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_a,T_a) )).

cnf(cls_power__less__power__Suc_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_Power_Opower__class_Opower(V_a,V_n,T_a),c_HOL_Otimes__class_Otimes(V_a,c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),V_a,T_a) )).

cnf(cls_power__minus_0,axiom,
    ( ~ class_Ring__and__Field_Oring__1(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Ouminus__class_Ouminus(V_a,T_a),V_n,T_a) = c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(c_HOL_Ouminus__class_Ouminus(c_HOL_Oone__class_Oone(T_a),T_a),V_n,T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a) )).

cnf(cls_division__ring__inverse__diff_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Ominus__class_Ominus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Ominus__class_Ominus(V_b,V_a,T_a),T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__le__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oone__class_Oone(T_a),V_x,T_a)
    | c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a)
    | ~ c_lessequals(c_HOL_Oinverse__class_Oinverse(V_x,T_a),c_HOL_Oone__class_Oone(T_a),T_a) )).

cnf(cls_inverse__le__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_lessequals(c_HOL_Oinverse__class_Oinverse(V_x,T_a),c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_lessequals(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_one__less__inverse__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),c_HOL_Oinverse__class_Oinverse(V_x,T_a),T_a) )).

cnf(cls_nonzero__inverse__eq__divide_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) = c_HOL_Oinverse__class_Odivide(c_HOL_Oone__class_Oone(T_a),V_a,T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_one__less__inverse__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__field(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Oone__class_Oone(T_a),c_HOL_Oinverse__class_Oinverse(V_x,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_x,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) )).

cnf(cls_poly__pCons_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Opoly(c_Polynomial_OpCons(V_a,V_p,T_a),V_x,T_a) = c_HOL_Oplus__class_Oplus(V_a,c_HOL_Otimes__class_Otimes(V_x,c_Polynomial_Opoly(V_p,V_x,T_a),T_a),T_a) )).

cnf(cls_nonzero__norm__inverse_0,axiom,
    ( ~ class_RealVector_Oreal__normed__div__algebra(T_a)
    | c_RealVector_Onorm__class_Onorm(c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) = c_HOL_Oinverse__class_Oinverse(c_RealVector_Onorm__class_Onorm(V_a,T_a),tc_RealDef_Oreal)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_poly__monom_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Opoly(c_Polynomial_Omonom(V_a,V_n,T_a),V_x,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_Power_Opower__class_Opower(V_x,V_n,T_a),T_a) )).

cnf(cls_pdivmod__rel__smult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Polynomial_Opdivmod__rel(V_x,c_Polynomial_Osmult(V_a,V_y,T_a),c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(V_a,T_a),V_q,T_a),V_r,T_a)
    | ~ c_Polynomial_Opdivmod__rel(V_x,V_y,V_q,V_r,T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_div__smult__right_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_Divides_Odiv__class_Odiv(V_x,c_Polynomial_Osmult(V_a,V_y,T_a),tc_Polynomial_Opoly(T_a)) = c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_Divides_Odiv__class_Odiv(V_x,V_y,tc_Polynomial_Opoly(T_a)),T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mrmq__eq_1,axiom,
    ( c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),V_w,tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_HOL_Oone__class_Oone(tc_RealDef_Oreal),tc_RealDef_Oreal)
    | ~ c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(v_q____,V_w,tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_RealDef_Oreal) )).

cnf(cls_mrmq__eq_0,axiom,
    ( c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(v_q____,V_w,tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_RealDef_Oreal)
    | ~ c_HOL_Oord__class_Oless(c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),V_w,tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_HOL_Oone__class_Oone(tc_RealDef_Oreal),tc_RealDef_Oreal) )).

cnf(cls_power__Suc__less_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__semidom(T_a)
    | c_HOL_Oord__class_Oless(c_HOL_Otimes__class_Otimes(V_a,c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a),c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(V_a,c_HOL_Oone__class_Oone(T_a),T_a)
    | ~ c_HOL_Oord__class_Oless(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) )).

cnf(cls_lgqr_0,axiom,
    ( c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(v_q____,tc_Complex_Ocomplex) = c_Fundamental__Theorem__Algebra__Mirabelle_Opsize(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_kas_I4_J_0,axiom,
    ( c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),V_z,tc_Complex_Ocomplex) = c_HOL_Oplus__class_Oplus(c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_z,v_k____,tc_Complex_Ocomplex),c_Polynomial_Opoly(c_Polynomial_OpCons(v_a____,v_s____,tc_Complex_Ocomplex),V_z,tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_class__semiring_Osemiring__rules_I9_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I10_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__left_Ozero_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_y,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult_Ozero__left_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult_Ozero__right_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__right_Ozero_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__semiring_Omul__0_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__zero__left_0,axiom,
    ( ~ class_Ring__and__Field_Omult__zero(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__zero__right_0,axiom,
    ( ~ class_Ring__and__Field_Omult__zero(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__eq__0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oring__no__zero__divisors(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),V_b,T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__eq__0__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oring__no__zero__divisors(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I2_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_m,T_a),V_m,T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oone__class_Oone(T_a),T_a),V_m,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I3_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_m,c_HOL_Otimes__class_Otimes(V_a,V_m,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oone__class_Oone(T_a),T_a),V_m,T_a) )).

cnf(cls_inverse__eq__1__iff_1,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Oone__class_Oone(T_a),T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_inverse__1_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Oone__class_Oone(T_a),T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_inverse__nonzero__iff__nonzero_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__zero__imp__zero_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_sum__squares__eq__zero__iff_2,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a),c_HOL_Otimes__class_Otimes(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_nonzero__power__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a) = c_Power_Opower__class_Opower(c_HOL_Oinverse__class_Oinverse(V_a,T_a),V_n,T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__ringb_Oadd__mul__solve_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_w,V_y,T_a),c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),T_a) != c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_w,V_z,T_a),c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a)
    | V_y = V_z
    | V_w = V_x )).

cnf(cls_class__ringb_Onoteq__reduce_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_d,T_a),T_a) != c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_d,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a)
    | V_c = V_d
    | V_a = V_b )).

cnf(cls_class__semiring_Osemiring__rules_I4_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_m,V_m,T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(c_HOL_Oone__class_Oone(T_a),c_HOL_Oone__class_Oone(T_a),T_a),V_m,T_a) )).

cnf(cls_power__commutes_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__mult(T_a)
    | c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_a,V_n,T_a),V_a,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a) )).

cnf(cls_comm__monoid__add_Omult__commute_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_b,T_a) = c_HOL_Oplus__class_Oplus(V_b,V_a,T_a) )).

cnf(cls_class__semiring_Oadd__c_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,V_y,T_a) = c_HOL_Oplus__class_Oplus(V_y,V_x,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I24_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_c,T_a) = c_HOL_Oplus__class_Oplus(V_c,V_a,T_a) )).

cnf(cls_qr_0,axiom,
    ( c_Polynomial_Opoly(v_q____,V_z,tc_Complex_Ocomplex) = c_HOL_Otimes__class_Otimes(c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),V_z,tc_Complex_Ocomplex),c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_add__cancel__21_0,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,c_HOL_Oplus__class_Oplus(V_y,V_z,T_a),T_a) != c_HOL_Oplus__class_Oplus(V_y,V_u,T_a)
    | c_HOL_Oplus__class_Oplus(V_x,V_z,T_a) = V_u )).

cnf(cls_power__one_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__mult(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Oone__class_Oone(T_a),V_n,T_a) = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_kas_I1_J_0,axiom,
    ( v_a____ != c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex) )).

cnf(cls_zero__neq__one_0,axiom,
    ( ~ class_Ring__and__Field_Ozero__neq__one(T_a)
    | c_HOL_Ozero__class_Ozero(T_a) != c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_nonzero__inverse__inverse__eq_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) = V_a
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_nonzero__inverse__mult__distrib_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_b,T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__mult__distrib_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) )).

cnf(cls_add__right__cancel_0,axiom,
    ( ~ class_OrderedGroup_Ocancel__semigroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_b,V_a,T_a) != c_HOL_Oplus__class_Oplus(V_c,V_a,T_a)
    | V_b = V_c )).

cnf(cls_add__left__cancel_0,axiom,
    ( ~ class_OrderedGroup_Ocancel__semigroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_b,T_a) != c_HOL_Oplus__class_Oplus(V_a,V_c,T_a)
    | V_b = V_c )).

cnf(cls_add__imp__eq_0,axiom,
    ( ~ class_OrderedGroup_Ocancel__ab__semigroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_b,T_a) != c_HOL_Oplus__class_Oplus(V_a,V_c,T_a)
    | V_b = V_c )).

cnf(cls_class__ringb_Oadd__cancel_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,V_y,T_a) != c_HOL_Oplus__class_Oplus(V_x,V_z,T_a)
    | V_y = V_z )).

cnf(cls_class__semiring_Osemiring__rules_I20_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Oplus__class_Oplus(V_c,V_d,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),c_HOL_Oplus__class_Oplus(V_b,V_d,T_a),T_a) )).

cnf(cls_division__ring__inverse__add_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__add_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oinverse__class_Oinverse(V_a,T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a),c_HOL_Oinverse__class_Oinverse(V_b,T_a),T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_a00_0,axiom,
    ( c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex) != c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex) )).

cnf(cls_smult__1__left_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Polynomial_Osmult(c_HOL_Oone__class_Oone(T_a),V_p,T_a) = V_p )).

cnf(cls_right__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) = c_HOL_Oone__class_Oone(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_left__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),V_a,T_a) = c_HOL_Oone__class_Oone(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_field__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oinverse__class_Oinverse(V_a,T_a),V_a,T_a) = c_HOL_Oone__class_Oone(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I1_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_m,T_a),c_HOL_Otimes__class_Otimes(V_b,V_m,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_m,T_a) )).

cnf(cls_comm__semiring__class_Odistrib_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_class__semiring_Omul__d_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,c_HOL_Oplus__class_Oplus(V_y,V_z,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),T_a) )).

cnf(cls_mult_Oadd__left_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,V_a_H,T_a),V_b,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Otimes__class_Otimes(V_a_H,V_b,T_a),T_a) )).

cnf(cls_mult__left_Oadd_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_x,V_y,T_a),V_ya,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_ya,T_a),c_HOL_Otimes__class_Otimes(V_y,V_ya,T_a),T_a) )).

cnf(cls_mult_Oadd__right_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Oplus__class_Oplus(V_b,V_b_H,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),c_HOL_Otimes__class_Otimes(V_a,V_b_H,T_a),T_a) )).

cnf(cls_mult__right_Oadd_0,axiom,
    ( ~ class_RealVector_Oreal__normed__algebra(T_a)
    | c_HOL_Otimes__class_Otimes(V_xa,c_HOL_Oplus__class_Oplus(V_x,V_y,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_xa,V_x,T_a),c_HOL_Otimes__class_Otimes(V_xa,V_y,T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I8_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_c,T_a),c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_class__semiring_Omul__a_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,c_HOL_Otimes__class_Otimes(V_y,V_z,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),V_z,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I19_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_lx,c_HOL_Otimes__class_Otimes(V_rx,V_ry,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_rx,c_HOL_Otimes__class_Otimes(V_lx,V_ry,T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I18_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_lx,c_HOL_Otimes__class_Otimes(V_rx,V_ry,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_rx,T_a),V_ry,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I17_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_ly,T_a),V_rx,T_a) = c_HOL_Otimes__class_Otimes(V_lx,c_HOL_Otimes__class_Otimes(V_ly,V_rx,T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I16_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_ly,T_a),V_rx,T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_rx,T_a),V_ly,T_a) )).

cnf(cls_ab__semigroup__mult__class_Omult__ac_I1_J_0,axiom,
    ( ~ class_OrderedGroup_Oab__semigroup__mult(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_c,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_HOL_Otimes__class_Otimes(V_b,V_c,T_a),T_a) )).

cnf(cls_power__inverse_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_Power_Opower__class_Opower(V_a,V_n,T_a),T_a) = c_Power_Opower__class_Opower(c_HOL_Oinverse__class_Oinverse(V_a,T_a),V_n,T_a) )).

cnf(cls_power__mult__distrib_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__mult(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_n,T_a) = c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_a,V_n,T_a),c_Power_Opower__class_Opower(V_b,V_n,T_a),T_a) )).

cnf(cls_class__semiring_Opwr__mul_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_Power_Opower__class_Opower(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),V_q,T_a) = c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(V_x,V_q,T_a),c_Power_Opower__class_Opower(V_y,V_q,T_a),T_a) )).

cnf(cls_inverse__zero_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__nonzero__iff__nonzero_1,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_nonzero__inverse__eq__imp__eq_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) != c_HOL_Oinverse__class_Oinverse(V_b,T_a)
    | V_a = V_b
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_inverse__eq__iff__eq_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) != c_HOL_Oinverse__class_Oinverse(V_b,T_a)
    | V_a = V_b )).

cnf(cls_mult__idem_0,axiom,
    ( ~ class_OrderedGroup_Oab__semigroup__idem__mult(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,V_x,T_a) = V_x )).

cnf(cls_inverse__unique_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,V_b,T_a) != c_HOL_Oone__class_Oone(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_a,T_a) = V_b )).

cnf(cls_sum__squares__eq__zero__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_y = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_sum__squares__eq__zero__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oordered__ring__strict(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_x,T_a),c_HOL_Otimes__class_Otimes(V_y,V_y,T_a),T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_x = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__ringb_Oadd__scale__eq__noteq_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,c_HOL_Otimes__class_Otimes(V_r,V_c,T_a),T_a) != c_HOL_Oplus__class_Oplus(V_x,c_HOL_Otimes__class_Otimes(V_r,V_d,T_a),T_a)
    | V_c = V_d
    | V_r = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_field__power__not__zero_0,axiom,
    ( ~ class_Ring__and__Field_Oring__1__no__zero__divisors(T_a)
    | c_Power_Opower__class_Opower(V_a,V_n,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_power__eq__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ozero__neq__one(T_a)
    | ~ class_Ring__and__Field_Ono__zero__divisors(T_a)
    | ~ class_Ring__and__Field_Omult__zero(T_a)
    | ~ class_Power_Opower(T_a)
    | c_Power_Opower__class_Opower(V_a,V_n,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__left__idem_0,axiom,
    ( ~ class_OrderedGroup_Oab__semigroup__idem__mult(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_x,V_y,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I15_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_ly,T_a),c_HOL_Otimes__class_Otimes(V_rx,V_ry,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_rx,c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_ly,T_a),V_ry,T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I14_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_ly,T_a),c_HOL_Otimes__class_Otimes(V_rx,V_ry,T_a),T_a) = c_HOL_Otimes__class_Otimes(V_lx,c_HOL_Otimes__class_Otimes(V_ly,c_HOL_Otimes__class_Otimes(V_rx,V_ry,T_a),T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I13_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_ly,T_a),c_HOL_Otimes__class_Otimes(V_rx,V_ry,T_a),T_a) = c_HOL_Otimes__class_Otimes(c_HOL_Otimes__class_Otimes(V_lx,V_rx,T_a),c_HOL_Otimes__class_Otimes(V_ly,V_ry,T_a),T_a) )).

cnf(cls_one__neq__zero_0,axiom,
    ( ~ class_Ring__and__Field_Ozero__neq__one(T_a)
    | c_HOL_Oone__class_Oone(T_a) != c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__eq__0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oring__no__zero__divisors(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_no__zero__divisors_0,axiom,
    ( ~ class_Ring__and__Field_Ono__zero__divisors(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_no__zero__divirors__neq0_0,axiom,
    ( ~ class_Ring__and__Field_Ono__zero__divisors(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,V_b,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a)
    | V_b = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_double__eq__0__iff_0,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,V_a,T_a) != c_HOL_Ozero__class_Ozero(T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__ringb_Oadd__r0__iff_0,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | V_x != c_HOL_Oplus__class_Oplus(V_x,V_a,T_a)
    | V_a = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_mult__1__right_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__mult(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Oone__class_Oone(T_a),T_a) = V_a )).

cnf(cls_mult__1__left_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__mult(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oone__class_Oone(T_a),V_a,T_a) = V_a )).

cnf(cls_mult__1_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__mult(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oone__class_Oone(T_a),V_a,T_a) = V_a )).

cnf(cls_class__semiring_Omul__1_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oone__class_Oone(T_a),V_x,T_a) = V_x )).

cnf(cls_class__semiring_Osemiring__rules_I12_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,c_HOL_Oone__class_Oone(T_a),T_a) = V_a )).

cnf(cls_class__semiring_Osemiring__rules_I11_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(c_HOL_Oone__class_Oone(T_a),V_a,T_a) = V_a )).

cnf(cls_class__ringb_Oadd__mul__solve_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_z,T_a),c_HOL_Otimes__class_Otimes(V_x,V_y,T_a),T_a) )).

cnf(cls_class__ringb_Onoteq__reduce_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_c,T_a),c_HOL_Otimes__class_Otimes(V_x,V_d,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_x,V_d,T_a),c_HOL_Otimes__class_Otimes(V_x,V_c,T_a),T_a) )).

cnf(cls_inverse__inverse__eq_0,axiom,
    ( ~ class_Ring__and__Field_Odivision__ring(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(c_HOL_Oinverse__class_Oinverse(V_a,T_a),T_a) = V_a )).

cnf(cls_class__semiring_Omul__c_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_x,V_y,T_a) = c_HOL_Otimes__class_Otimes(V_y,V_x,T_a) )).

cnf(cls_double__eq__0__iff_1,axiom,
    ( ~ class_OrderedGroup_Olordered__ab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(T_a),c_HOL_Ozero__class_Ozero(T_a),T_a) = c_HOL_Ozero__class_Ozero(T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I7_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Otimes__class_Otimes(V_a,V_b,T_a) = c_HOL_Otimes__class_Otimes(V_b,V_a,T_a) )).

cnf(cls_comm__monoid__add_Omult__left__commute_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) = c_HOL_Oplus__class_Oplus(V_b,c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),T_a) )).

cnf(cls_comm__monoid__add_Omult__assoc_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) )).

cnf(cls_class__semiring_Oadd__a_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,c_HOL_Oplus__class_Oplus(V_y,V_z,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_x,V_y,T_a),V_z,T_a) )).

cnf(cls_add__cancel__21_1,axiom,
    ( ~ class_OrderedGroup_Oab__group__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_x,c_HOL_Oplus__class_Oplus(V_y,V_z,T_a),T_a) = c_HOL_Oplus__class_Oplus(V_y,c_HOL_Oplus__class_Oplus(V_x,V_z,T_a),T_a) )).

cnf(cls_ab__semigroup__add__class_Oadd__ac_I1_J_0,axiom,
    ( ~ class_OrderedGroup_Oab__semigroup__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I25_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(V_c,V_d,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),V_d,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I23_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_c,T_a),V_b,T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I22_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(V_c,V_d,T_a),T_a) = c_HOL_Oplus__class_Oplus(V_c,c_HOL_Oplus__class_Oplus(V_a,V_d,T_a),T_a) )).

cnf(cls_class__semiring_Osemiring__rules_I21_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_c,T_a) = c_HOL_Oplus__class_Oplus(V_a,c_HOL_Oplus__class_Oplus(V_b,V_c,T_a),T_a) )).

cnf(cls_combine__common__factor_0,axiom,
    ( ~ class_Ring__and__Field_Osemiring(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_a,V_e,T_a),c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(V_b,V_e,T_a),V_c,T_a),T_a) = c_HOL_Oplus__class_Oplus(c_HOL_Otimes__class_Otimes(c_HOL_Oplus__class_Oplus(V_a,V_b,T_a),V_e,T_a),V_c,T_a) )).

cnf(cls_inverse__eq__1__iff_0,axiom,
    ( ~ class_Ring__and__Field_Ofield(T_a)
    | ~ class_Ring__and__Field_Odivision__by__zero(T_a)
    | c_HOL_Oinverse__class_Oinverse(V_x,T_a) != c_HOL_Oone__class_Oone(T_a)
    | V_x = c_HOL_Oone__class_Oone(T_a) )).

cnf(cls_r01_0,axiom,
    ( c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex) = c_HOL_Oone__class_Oone(tc_Complex_Ocomplex) )).

cnf(cls_poly__smult_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Opoly(c_Polynomial_Osmult(V_a,V_p,T_a),V_x,T_a) = c_HOL_Otimes__class_Otimes(V_a,c_Polynomial_Opoly(V_p,V_x,T_a),T_a) )).

cnf(cls_smult__smult_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__0(T_a)
    | c_Polynomial_Osmult(V_a,c_Polynomial_Osmult(V_b,V_p,T_a),T_a) = c_Polynomial_Osmult(c_HOL_Otimes__class_Otimes(V_a,V_b,T_a),V_p,T_a) )).

cnf(cls_monoid__add__class_Oadd__0__right_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = V_a )).

cnf(cls_add__0__left_0,axiom,
    ( ~ class_OrderedGroup_Omonoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = V_a )).

cnf(cls_comm__monoid__add_Omult__1__right_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = V_a )).

cnf(cls_class__ringb_Oadd__r0__iff_1,axiom,
    ( ~ class_Ring__and__Field_Oidom(T_a)
    | ~ class_Int_Onumber__ring(T_a)
    | V_x = c_HOL_Oplus__class_Oplus(V_x,c_HOL_Ozero__class_Ozero(T_a),T_a) )).

cnf(cls_class__semiring_Oadd__0_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(T_a),V_x,T_a) = V_x )).

cnf(cls_comm__monoid__add_Omult__1_0,axiom,
    ( ~ class_OrderedGroup_Ocomm__monoid__add(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = V_a )).

cnf(cls_class__semiring_Osemiring__rules_I6_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(V_a,c_HOL_Ozero__class_Ozero(T_a),T_a) = V_a )).

cnf(cls_class__semiring_Osemiring__rules_I5_J_0,axiom,
    ( ~ class_Ring__and__Field_Ocomm__semiring__1(T_a)
    | c_HOL_Oplus__class_Oplus(c_HOL_Ozero__class_Ozero(T_a),V_a,T_a) = V_a )).

cnf(cls_CHAINED_0,axiom,
    ( c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex) = c_HOL_Oone__class_Oone(tc_Complex_Ocomplex) )).

cnf(cls_CHAINED_0_01,axiom,
    ( v_s____ = c_HOL_Ozero__class_Ozero(tc_Polynomial_Opoly(tc_Complex_Ocomplex)) )).

cnf(cls_CHAINED_0_02,axiom,
    ( c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),v_w____,tc_Complex_Ocomplex) = c_HOL_Oplus__class_Oplus(c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),c_HOL_Otimes__class_Otimes(c_Power_Opower__class_Opower(v_w____,v_k____,tc_Complex_Ocomplex),c_Polynomial_Opoly(c_Polynomial_OpCons(v_a____,v_s____,tc_Complex_Ocomplex),v_w____,tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(cls_conjecture_0,negated_conjecture,
    ( c_RealVector_Onorm__class_Onorm(c_Polynomial_Opoly(c_Polynomial_Osmult(c_HOL_Oinverse__class_Oinverse(c_Polynomial_Opoly(v_q____,c_HOL_Ozero__class_Ozero(tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),v_q____,tc_Complex_Ocomplex),v_w____,tc_Complex_Ocomplex),tc_Complex_Ocomplex) != c_RealVector_Onorm__class_Onorm(c_HOL_Oplus__class_Oplus(c_HOL_Oone__class_Oone(tc_Complex_Ocomplex),c_HOL_Otimes__class_Otimes(v_a____,c_Power_Opower__class_Opower(v_w____,v_k____,tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex),tc_Complex_Ocomplex) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Ocancel__comm__monoid__add,axiom,
    ( class_OrderedGroup_Ocancel__comm__monoid__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Ocancel__comm__monoid__add(T_1) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Ocancel__comm__monoid__add,axiom,
    ( class_OrderedGroup_Ocancel__comm__monoid__add(tc_Complex_Ocomplex) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Ocancel__comm__monoid__add,axiom,
    ( class_OrderedGroup_Ocancel__comm__monoid__add(tc_RealDef_Oreal) )).

cnf(clsarity_nat__OrderedGroup_Ocancel__comm__monoid__add,axiom,
    ( class_OrderedGroup_Ocancel__comm__monoid__add(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Opordered__cancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Opordered__cancel__ab__semigroup__add(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Opordered__ab__semigroup__add__imp__le,axiom,
    ( class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Oordered__comm__semiring__strict,axiom,
    ( class_Ring__and__Field_Oordered__comm__semiring__strict(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Opordered__cancel__semiring,axiom,
    ( class_Ring__and__Field_Opordered__cancel__semiring(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Oordered__semiring__strict,axiom,
    ( class_Ring__and__Field_Oordered__semiring__strict(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Opordered__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Opordered__ab__semigroup__add(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Opordered__comm__monoid__add,axiom,
    ( class_OrderedGroup_Opordered__comm__monoid__add(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Ocancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__ab__semigroup__add(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Ocancel__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__semigroup__add(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Opordered__semiring,axiom,
    ( class_Ring__and__Field_Opordered__semiring(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Oordered__semiring,axiom,
    ( class_Ring__and__Field_Oordered__semiring(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Ono__zero__divisors,axiom,
    ( class_Ring__and__Field_Ono__zero__divisors(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Oordered__semidom,axiom,
    ( class_Ring__and__Field_Oordered__semidom(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Ocomm__semiring__1,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__1(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Ocomm__semiring__0,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__0(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Oab__semigroup__mult,axiom,
    ( class_OrderedGroup_Oab__semigroup__mult(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Ocomm__monoid__mult,axiom,
    ( class_OrderedGroup_Ocomm__monoid__mult(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Oab__semigroup__add,axiom,
    ( class_OrderedGroup_Oab__semigroup__add(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Ocomm__semiring,axiom,
    ( class_Ring__and__Field_Ocomm__semiring(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Ocomm__monoid__add,axiom,
    ( class_OrderedGroup_Ocomm__monoid__add(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Ozero__neq__one,axiom,
    ( class_Ring__and__Field_Ozero__neq__one(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Osemiring__0,axiom,
    ( class_Ring__and__Field_Osemiring__0(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Omult__mono1,axiom,
    ( class_Ring__and__Field_Omult__mono1(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Omult__zero,axiom,
    ( class_Ring__and__Field_Omult__zero(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Omult__mono,axiom,
    ( class_Ring__and__Field_Omult__mono(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Omonoid__mult,axiom,
    ( class_OrderedGroup_Omonoid__mult(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Osemiring,axiom,
    ( class_Ring__and__Field_Osemiring(tc_nat) )).

cnf(clsarity_nat__OrderedGroup_Omonoid__add,axiom,
    ( class_OrderedGroup_Omonoid__add(tc_nat) )).

cnf(clsarity_nat__Divides_Osemiring__div,axiom,
    ( class_Divides_Osemiring__div(tc_nat) )).

cnf(clsarity_nat__Ring__and__Field_Odvd,axiom,
    ( class_Ring__and__Field_Odvd(tc_nat) )).

cnf(clsarity_nat__Orderings_Opreorder,axiom,
    ( class_Orderings_Opreorder(tc_nat) )).

cnf(clsarity_nat__Orderings_Olinorder,axiom,
    ( class_Orderings_Olinorder(tc_nat) )).

cnf(clsarity_nat__Orderings_Oorder,axiom,
    ( class_Orderings_Oorder(tc_nat) )).

cnf(clsarity_nat__Power_Opower,axiom,
    ( class_Power_Opower(tc_nat) )).

cnf(clsarity_nat__HOL_Ozero,axiom,
    ( class_HOL_Ozero(tc_nat) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Opordered__cancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Opordered__cancel__ab__semigroup__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Opordered__ab__semigroup__add__imp__le,axiom,
    ( class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__comm__semiring__strict,axiom,
    ( class_Ring__and__Field_Oordered__comm__semiring__strict(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Opordered__cancel__semiring,axiom,
    ( class_Ring__and__Field_Opordered__cancel__semiring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oring__1__no__zero__divisors,axiom,
    ( class_Ring__and__Field_Oring__1__no__zero__divisors(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__semiring__strict,axiom,
    ( class_Ring__and__Field_Oordered__semiring__strict(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Opordered__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Opordered__ab__semigroup__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Opordered__comm__monoid__add,axiom,
    ( class_OrderedGroup_Opordered__comm__monoid__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oring__no__zero__divisors,axiom,
    ( class_Ring__and__Field_Oring__no__zero__divisors(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Ocancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__ab__semigroup__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__ring__strict,axiom,
    ( class_Ring__and__Field_Oordered__ring__strict(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__RealVector_Oreal__normed__div__algebra,axiom,
    ( class_RealVector_Oreal__normed__div__algebra(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Opordered__ab__group__add,axiom,
    ( class_OrderedGroup_Opordered__ab__group__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Olordered__ab__group__add,axiom,
    ( class_OrderedGroup_Olordered__ab__group__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Oordered__ab__group__add,axiom,
    ( class_OrderedGroup_Oordered__ab__group__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Ocancel__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__semigroup__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Opordered__semiring,axiom,
    ( class_Ring__and__Field_Opordered__semiring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__RealVector_Oreal__normed__algebra__1,axiom,
    ( class_RealVector_Oreal__normed__algebra__1(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__semiring,axiom,
    ( class_Ring__and__Field_Oordered__semiring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ono__zero__divisors,axiom,
    ( class_Ring__and__Field_Ono__zero__divisors(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Odivision__by__zero,axiom,
    ( class_Ring__and__Field_Odivision__by__zero(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__semidom,axiom,
    ( class_Ring__and__Field_Oordered__semidom(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ocomm__semiring__1,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__1(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ocomm__semiring__0,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__0(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__RealVector_Oreal__normed__algebra,axiom,
    ( class_RealVector_Oreal__normed__algebra(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Oab__semigroup__mult,axiom,
    ( class_OrderedGroup_Oab__semigroup__mult(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__RealVector_Oreal__normed__vector,axiom,
    ( class_RealVector_Oreal__normed__vector(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Ocomm__monoid__mult,axiom,
    ( class_OrderedGroup_Ocomm__monoid__mult(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Oab__semigroup__add,axiom,
    ( class_OrderedGroup_Oab__semigroup__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Opordered__ring,axiom,
    ( class_Ring__and__Field_Opordered__ring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__field,axiom,
    ( class_Ring__and__Field_Oordered__field(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Odivision__ring,axiom,
    ( class_Ring__and__Field_Odivision__ring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ocomm__semiring,axiom,
    ( class_Ring__and__Field_Ocomm__semiring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__RealVector_Oreal__normed__field,axiom,
    ( class_RealVector_Oreal__normed__field(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Ocomm__monoid__add,axiom,
    ( class_OrderedGroup_Ocomm__monoid__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ozero__neq__one,axiom,
    ( class_Ring__and__Field_Ozero__neq__one(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oordered__idom,axiom,
    ( class_Ring__and__Field_Oordered__idom(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ocomm__ring__1,axiom,
    ( class_Ring__and__Field_Ocomm__ring__1(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Osemiring__0,axiom,
    ( class_Ring__and__Field_Osemiring__0(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Omult__mono1,axiom,
    ( class_Ring__and__Field_Omult__mono1(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Oab__group__add,axiom,
    ( class_OrderedGroup_Oab__group__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Omult__zero,axiom,
    ( class_Ring__and__Field_Omult__zero(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Omult__mono,axiom,
    ( class_Ring__and__Field_Omult__mono(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ocomm__ring,axiom,
    ( class_Ring__and__Field_Ocomm__ring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Omonoid__mult,axiom,
    ( class_OrderedGroup_Omonoid__mult(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Osemiring,axiom,
    ( class_Ring__and__Field_Osemiring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Omonoid__add,axiom,
    ( class_OrderedGroup_Omonoid__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__OrderedGroup_Ogroup__add,axiom,
    ( class_OrderedGroup_Ogroup__add(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oring__1,axiom,
    ( class_Ring__and__Field_Oring__1(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__RealVector_Oreal__field,axiom,
    ( class_RealVector_Oreal__field(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Ofield,axiom,
    ( class_Ring__and__Field_Ofield(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oring,axiom,
    ( class_Ring__and__Field_Oring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Oidom,axiom,
    ( class_Ring__and__Field_Oidom(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Ring__and__Field_Odvd,axiom,
    ( class_Ring__and__Field_Odvd(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Orderings_Opreorder,axiom,
    ( class_Orderings_Opreorder(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Orderings_Olinorder,axiom,
    ( class_Orderings_Olinorder(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Orderings_Oorder,axiom,
    ( class_Orderings_Oorder(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Int_Onumber__ring,axiom,
    ( class_Int_Onumber__ring(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__Power_Opower,axiom,
    ( class_Power_Opower(tc_RealDef_Oreal) )).

cnf(clsarity_RealDef__Oreal__HOL_Ozero,axiom,
    ( class_HOL_Ozero(tc_RealDef_Oreal) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Oring__1__no__zero__divisors,axiom,
    ( class_Ring__and__Field_Oring__1__no__zero__divisors(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Oring__no__zero__divisors,axiom,
    ( class_Ring__and__Field_Oring__no__zero__divisors(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Ocancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__ab__semigroup__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__RealVector_Oreal__normed__div__algebra,axiom,
    ( class_RealVector_Oreal__normed__div__algebra(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Ocancel__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__semigroup__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__RealVector_Oreal__normed__algebra__1,axiom,
    ( class_RealVector_Oreal__normed__algebra__1(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ono__zero__divisors,axiom,
    ( class_Ring__and__Field_Ono__zero__divisors(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Odivision__by__zero,axiom,
    ( class_Ring__and__Field_Odivision__by__zero(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ocomm__semiring__1,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__1(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ocomm__semiring__0,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__0(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__RealVector_Oreal__normed__algebra,axiom,
    ( class_RealVector_Oreal__normed__algebra(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Oab__semigroup__mult,axiom,
    ( class_OrderedGroup_Oab__semigroup__mult(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__RealVector_Oreal__normed__vector,axiom,
    ( class_RealVector_Oreal__normed__vector(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Ocomm__monoid__mult,axiom,
    ( class_OrderedGroup_Ocomm__monoid__mult(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Oab__semigroup__add,axiom,
    ( class_OrderedGroup_Oab__semigroup__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Odivision__ring,axiom,
    ( class_Ring__and__Field_Odivision__ring(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ocomm__semiring,axiom,
    ( class_Ring__and__Field_Ocomm__semiring(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__RealVector_Oreal__normed__field,axiom,
    ( class_RealVector_Oreal__normed__field(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Ocomm__monoid__add,axiom,
    ( class_OrderedGroup_Ocomm__monoid__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ozero__neq__one,axiom,
    ( class_Ring__and__Field_Ozero__neq__one(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ocomm__ring__1,axiom,
    ( class_Ring__and__Field_Ocomm__ring__1(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Osemiring__0,axiom,
    ( class_Ring__and__Field_Osemiring__0(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Oab__group__add,axiom,
    ( class_OrderedGroup_Oab__group__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Omult__zero,axiom,
    ( class_Ring__and__Field_Omult__zero(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ocomm__ring,axiom,
    ( class_Ring__and__Field_Ocomm__ring(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Omonoid__mult,axiom,
    ( class_OrderedGroup_Omonoid__mult(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Osemiring,axiom,
    ( class_Ring__and__Field_Osemiring(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Omonoid__add,axiom,
    ( class_OrderedGroup_Omonoid__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__OrderedGroup_Ogroup__add,axiom,
    ( class_OrderedGroup_Ogroup__add(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Oring__1,axiom,
    ( class_Ring__and__Field_Oring__1(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__RealVector_Oreal__field,axiom,
    ( class_RealVector_Oreal__field(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Ofield,axiom,
    ( class_Ring__and__Field_Ofield(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Oring,axiom,
    ( class_Ring__and__Field_Oring(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Oidom,axiom,
    ( class_Ring__and__Field_Oidom(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Ring__and__Field_Odvd,axiom,
    ( class_Ring__and__Field_Odvd(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Int_Onumber__ring,axiom,
    ( class_Int_Onumber__ring(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__Power_Opower,axiom,
    ( class_Power_Opower(tc_Complex_Ocomplex) )).

cnf(clsarity_Complex__Ocomplex__HOL_Ozero,axiom,
    ( class_HOL_Ozero(tc_Complex_Ocomplex) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Opordered__cancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Opordered__cancel__ab__semigroup__add(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Opordered__ab__semigroup__add__imp__le,axiom,
    ( class_OrderedGroup_Opordered__ab__semigroup__add__imp__le(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oordered__comm__semiring__strict,axiom,
    ( class_Ring__and__Field_Oordered__comm__semiring__strict(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Opordered__cancel__semiring,axiom,
    ( class_Ring__and__Field_Opordered__cancel__semiring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oring__1__no__zero__divisors,axiom,
    ( class_Ring__and__Field_Oring__1__no__zero__divisors(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oidom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oordered__semiring__strict,axiom,
    ( class_Ring__and__Field_Oordered__semiring__strict(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Opordered__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Opordered__ab__semigroup__add(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Opordered__comm__monoid__add,axiom,
    ( class_OrderedGroup_Opordered__comm__monoid__add(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oring__no__zero__divisors,axiom,
    ( class_Ring__and__Field_Oring__no__zero__divisors(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oidom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Ocancel__ab__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__ab__semigroup__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Ocancel__comm__monoid__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oordered__ring__strict,axiom,
    ( class_Ring__and__Field_Oordered__ring__strict(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Opordered__ab__group__add,axiom,
    ( class_OrderedGroup_Opordered__ab__group__add(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Oordered__ab__group__add,axiom,
    ( class_OrderedGroup_Oordered__ab__group__add(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Ocancel__semigroup__add,axiom,
    ( class_OrderedGroup_Ocancel__semigroup__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Ocancel__comm__monoid__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Opordered__semiring,axiom,
    ( class_Ring__and__Field_Opordered__semiring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oordered__semiring,axiom,
    ( class_Ring__and__Field_Oordered__semiring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ono__zero__divisors,axiom,
    ( class_Ring__and__Field_Ono__zero__divisors(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oidom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oordered__semidom,axiom,
    ( class_Ring__and__Field_Oordered__semidom(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ocomm__semiring__1,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__1(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ocomm__semiring__0,axiom,
    ( class_Ring__and__Field_Ocomm__semiring__0(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__0(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Oab__semigroup__mult,axiom,
    ( class_OrderedGroup_Oab__semigroup__mult(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__0(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Ocomm__monoid__mult,axiom,
    ( class_OrderedGroup_Ocomm__monoid__mult(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Oab__semigroup__add,axiom,
    ( class_OrderedGroup_Oab__semigroup__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Opordered__ring,axiom,
    ( class_Ring__and__Field_Opordered__ring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ocomm__semiring,axiom,
    ( class_Ring__and__Field_Ocomm__semiring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__0(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Ocomm__monoid__add,axiom,
    ( class_OrderedGroup_Ocomm__monoid__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ozero__neq__one,axiom,
    ( class_Ring__and__Field_Ozero__neq__one(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oordered__idom,axiom,
    ( class_Ring__and__Field_Oordered__idom(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ocomm__ring__1,axiom,
    ( class_Ring__and__Field_Ocomm__ring__1(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__ring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Osemiring__0,axiom,
    ( class_Ring__and__Field_Osemiring__0(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__0(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Omult__mono1,axiom,
    ( class_Ring__and__Field_Omult__mono1(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Oab__group__add,axiom,
    ( class_OrderedGroup_Oab__group__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Oab__group__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Omult__zero,axiom,
    ( class_Ring__and__Field_Omult__zero(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__0(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Omult__mono,axiom,
    ( class_Ring__and__Field_Omult__mono(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Ocomm__ring,axiom,
    ( class_Ring__and__Field_Ocomm__ring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__ring(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Omonoid__mult,axiom,
    ( class_OrderedGroup_Omonoid__mult(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Osemiring,axiom,
    ( class_Ring__and__Field_Osemiring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__0(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Omonoid__add,axiom,
    ( class_OrderedGroup_Omonoid__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Ocomm__monoid__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__OrderedGroup_Ogroup__add,axiom,
    ( class_OrderedGroup_Ogroup__add(tc_Polynomial_Opoly(T_1))
    | ~ class_OrderedGroup_Oab__group__add(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oring__1,axiom,
    ( class_Ring__and__Field_Oring__1(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__ring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Divides_Osemiring__div,axiom,
    ( class_Divides_Osemiring__div(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ofield(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oring,axiom,
    ( class_Ring__and__Field_Oring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__ring(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Oidom,axiom,
    ( class_Ring__and__Field_Oidom(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oidom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Ring__and__Field_Odvd,axiom,
    ( class_Ring__and__Field_Odvd(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Orderings_Opreorder,axiom,
    ( class_Orderings_Opreorder(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Orderings_Olinorder,axiom,
    ( class_Orderings_Olinorder(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Divides_Oring__div,axiom,
    ( class_Divides_Oring__div(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ofield(T_1) )).

cnf(clsarity_Polynomial__Opoly__Orderings_Oorder,axiom,
    ( class_Orderings_Oorder(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Oordered__idom(T_1) )).

cnf(clsarity_Polynomial__Opoly__Int_Onumber__ring,axiom,
    ( class_Int_Onumber__ring(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__ring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__Power_Opower,axiom,
    ( class_Power_Opower(tc_Polynomial_Opoly(T_1))
    | ~ class_Ring__and__Field_Ocomm__semiring__1(T_1) )).

cnf(clsarity_Polynomial__Opoly__HOL_Ozero,axiom,
    ( class_HOL_Ozero(tc_Polynomial_Opoly(T_1))
    | ~ class_HOL_Ozero(T_1) )).

%------------------------------------------------------------------------------
