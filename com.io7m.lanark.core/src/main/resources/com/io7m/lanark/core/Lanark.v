(*
 * Copyright © 2023 Mark Raynsford <code@io7m.com> https://www.io7m.com
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Arith.Peano_dec.
Require Import Coq.Arith.Arith_base.

Import ListNotations.

Local Open Scope string_scope.

Set Mangle Names.

(** The type of primary segments of a dotted name. *)
Record segmentPrimary : Set := MakeSegmentPrimary {
  siFirst : Ascii.ascii;
  siRest  : list Ascii.ascii;
}.

(** The type of secondary segments of a dotted name. *)
Record segmentSecondary : Set := MakeSegmentSecondary {
  stFirst : Ascii.ascii;
  stRest  : list Ascii.ascii;
}.

(** The set of characters that are acceptable for the start of a segment. *)
Definition acceptableFirstCharacters : list Ascii.ascii :=
  list_ascii_of_string "abcdefghijklmnopqrstuvwxyz".

(** The acceptable characters for the rest of a name segment. *)
Definition acceptableRestCharacters : list Ascii.ascii :=
  list_ascii_of_string "abcdefghijklmnopqrstuvwxyz0123456789-_".

(** A description of a valid primary name segment. *)
Definition validSegmentPrimary (s : segmentPrimary) : Prop :=
     In (siFirst s) acceptableFirstCharacters
  /\ Forall (fun c => In c acceptableRestCharacters) (siRest s)
  /\ List.length (siRest s) <= 63.

(** A description of a valid secondary name segment. *)
Definition validSegmentSecondary (s : segmentSecondary) : Prop :=
     In (stFirst s) acceptableFirstCharacters
  /\ Forall (fun c => In c acceptableRestCharacters) (stRest s)
  /\ List.length (stRest s) <= 62.

(** Whether all characters in a string are valid is decidable. *)
Lemma validSegmentRestForall_dec : forall s,
  {Forall (fun c => In c acceptableRestCharacters) s}+
  {~Forall (fun c => In c acceptableRestCharacters) s}.
Proof.
  intros s.
  apply Forall_dec.
  intros c.
  apply in_dec.
  apply Ascii.ascii_dec.
Qed.

(** Whether a string is short enough is decidable. *)
Lemma validNameLength63_dec : forall (s : list Ascii.ascii),
  {List.length s <= 63}+{~List.length s <= 63}.
Proof.
  intros s.
  apply Compare_dec.ge_dec.
Qed.

(** Whether a string is short enough is decidable. *)
Lemma validNameLength62_dec : forall (s : list Ascii.ascii),
  {List.length s <= 62}+{~List.length s <= 62}.
Proof.
  intros s.
  apply Compare_dec.ge_dec.
Qed.

(** Whether an primary name segment is valid is decidable. *)
Theorem validSegmentPrimaryDecidable : forall s,
  {validSegmentPrimary s}+{~validSegmentPrimary s}.
Proof.
  intros s.
  unfold validSegmentPrimary.
  destruct (in_dec ascii_dec (siFirst s) acceptableFirstCharacters); intuition.
  destruct (validSegmentRestForall_dec (siRest s)); intuition.
  destruct (validNameLength63_dec (siRest s)); intuition.
Qed.

(** Whether a secondary name segment is valid is decidable. *)
Theorem validSegmentSecondaryDecidable : forall s,
  {validSegmentSecondary s}+{~validSegmentSecondary s}.
Proof.
  intros s.
  unfold validSegmentSecondary.
  destruct (in_dec ascii_dec (stFirst s) acceptableFirstCharacters); intuition.
  destruct (validSegmentRestForall_dec (stRest s)); intuition.
  destruct (validNameLength62_dec (stRest s)); intuition.
Qed.

(** A valid secondary name segment is also a valid primary name segment. *)
Theorem validSegmentSecondaryPrimary : forall s,
  validSegmentSecondary s ->
    validSegmentPrimary (MakeSegmentPrimary (stFirst s) (stRest s)).
Proof.
  intros s Hs.
  unfold validSegmentPrimary.
  inversion Hs as [H0 [H1 H2]].
  split. {
    apply H0.
  } {
    split. {
      apply H1.
    } {
      apply (PeanoNat.Nat.le_trans _ _ 63 H2).
      auto.
    }
  }
Qed.

(** The type of dotted names. *)
Record name : Set := MakeName {
  namePrimary  : segmentPrimary;
  nameSecondary : list segmentSecondary
}.

(** A description of a valid dotted name. *)
Definition nameValid (n : name) : Prop :=
     validSegmentPrimary (namePrimary n)
  /\ Forall validSegmentSecondary (nameSecondary n)
  /\ List.length (nameSecondary n) <= 15.

(** Whether all characters in a string are valid is decidable. *)
Lemma nameValidSegmentSecondaryForall_dec : forall n,
  {Forall validSegmentSecondary n}+
  {~Forall validSegmentSecondary n}.
Proof.
  intros n.
  apply Forall_dec.
  intros s.
  apply validSegmentSecondaryDecidable.
Qed.

(** Whether a list of secondary names is short enough is decidable. *)
Lemma validNameLength15_dec : forall (s : list segmentSecondary),
  {List.length s <= 15}+{~List.length s <= 15}.
Proof.
  intros s.
  apply Compare_dec.ge_dec.
Qed.

(** Whether a name is valid is decidable. *)
Theorem nameValidDecidable : forall n,
  {nameValid n}+{~nameValid n}.
Proof.
  intros n.
  unfold nameValid.
  destruct (validSegmentPrimaryDecidable (namePrimary n)); intuition.
  destruct (nameValidSegmentSecondaryForall_dec (nameSecondary n)); intuition.
  destruct (validNameLength15_dec (nameSecondary n)); intuition.
Qed.

(** The class of objects that can be turned into strings. *)
Class ToString (A : Type) := {
  (** Turn the object into a string. *)
  toString : A -> string
}.

Definition tsSegmentPrimary (s : segmentPrimary) : string :=
  string_of_list_ascii (cons (siFirst s) (siRest s)).

Definition tsSegmentSecondary (s : segmentSecondary) : string :=
  string_of_list_ascii (cons (stFirst s) (stRest s)).

#[export]
Instance toStringSegmentPrimary : ToString segmentPrimary := {
  toString := tsSegmentPrimary
}.

#[export]
Instance toStringSegmentSecondary : ToString segmentSecondary := {
  toString := tsSegmentSecondary
}.

Lemma stringLengthEq : forall (s : list ascii),
  Datatypes.length s = length (string_of_list_ascii s).
Proof.
  intro s.
  induction s as [|y ys IHys]. {
    reflexivity.
  } {
    simpl.
    rewrite IHys.
    reflexivity.
  }
Qed.

Lemma toStringSegmentPrimaryLength : forall (s : segmentPrimary),
  validSegmentPrimary s -> length (toString s) <= 64.
Proof.
  intros s Hv.
  unfold toString.
  unfold toStringSegmentPrimary.
  unfold tsSegmentPrimary.
  inversion Hv as [H0 [H1 H2]].
  simpl.
  rewrite stringLengthEq in H2.
  intuition.
Qed.

Lemma toStringSegmentSecondaryLength : forall (s : segmentSecondary),
  validSegmentSecondary s -> length (toString s) <= 63.
Proof.
  intros s Hv.
  unfold toString.
  unfold toStringSegmentSecondary.
  unfold tsSegmentSecondary.
  inversion Hv as [H0 [H1 H2]].
  simpl.
  rewrite stringLengthEq in H2.
  intuition.
Qed.

Definition tsName (s : name) : string :=
  let ss      : list string := map toString (nameSecondary s) in
  let dotted  : list string := map (fun k => String.append "." k) ss in
  let dottedS : string      := fold_left String.append dotted "" in
    String.append (toString (namePrimary s)) dottedS.

#[export]
Instance toStringName : ToString name := {
  toString := tsName
}.

Lemma nameMapToStringForall : forall (n : name),
  nameValid n ->
    Forall (fun s => String.length s <= 63) (map toString (nameSecondary n)).
Proof.
  intros n [Hnv0 [Hnv1 Hnv2]].
  induction (nameSecondary n) as [|y ys IHys]. {
    constructor.
  } {
    constructor. {
      apply toStringSegmentSecondaryLength.
      apply (Forall_inv Hnv1).
    }
    apply IHys.
    apply (Forall_inv_tail Hnv1).
    intuition.
  }
Qed.

Lemma  nameMapDottedForall : forall xs,
  Forall (fun s => String.length s <= 63) xs ->
    Forall (fun s => String.length s <= 64) (map (fun k => String.append "." k) xs).
Proof.
  intro xs.
  induction xs as [|y ys IHys]. {
    constructor.
  } {
    intros Hfa1.
    simpl.
    constructor. {
      assert (length y <= 63) as H0. {
        apply (Forall_inv Hfa1).
      }
      simpl.
      intuition.
    } {
      apply IHys.
      apply (Forall_inv_tail Hfa1).
    }
  }
Qed.

Lemma stringAppendListLength : forall s t,
  String.length (String.append s t) =
    String.length s + String.length t.
Proof.
  intro s.
  induction s as [|z zs IHzs]. {
    reflexivity.
  } {
    simpl.
    intros t.
    rewrite IHzs.
    reflexivity.
  }
Qed.

Lemma foldAppendConsLength : forall ss s t,
  String.length (fold_left append (s :: ss) t) =
    (String.length s) + (String.length (fold_left append ss t)).
Proof.
  intro ss.
  induction ss as [|y ys IHys]. {
    intros s t.
    simpl.
    rewrite stringAppendListLength.
    rewrite PeanoNat.Nat.add_comm.
    reflexivity.
  } {
    intros s t.
    simpl in *.
    rewrite (IHys y t).
    rewrite (IHys y (t ++ s)).
    rewrite (IHys s t).
    remember (length (fold_left append ys t)) as L0.
    remember (length y) as L1.
    remember (length s) as L2.
    rewrite PeanoNat.Nat.add_assoc.
    rewrite PeanoNat.Nat.add_assoc.
    assert (L1 + L2 = L2 + L1) as H0 by apply PeanoNat.Nat.add_comm.
    rewrite H0.
    reflexivity.
  }
Qed.

Lemma foldAppendLength : forall xs n,
  Forall (fun s => String.length s <= n) xs ->
    String.length (fold_left append xs "") <= (List.length xs) * n.
Proof.
  intro xs.
  induction xs as [|y ys IHys]. {
    intuition.
  } {
    intros n Hfa.
    rewrite foldAppendConsLength.
    pose proof (IHys n (Forall_inv_tail Hfa)) as IH.
    clear IHys.
    simpl.
    pose proof (Forall_inv Hfa) as HlenA.
    simpl in HlenA.
    remember (List.length ys * n) as B.
    apply PeanoNat.Nat.add_le_mono.
    exact HlenA.
    exact IH.
  }
Qed.

Lemma nameSizeSecondary : forall (n : name),
  nameValid n ->
    String.length (fold_left append (map (fun k : string => "." ++ k) (map toString (nameSecondary n))) "") <= 960.
Proof.
  intros n Hnv.
  remember (map (fun k : string => "." ++ k) (map toString (nameSecondary n))) as ks.

  assert (List.length ks <= 15) as H0. {
    inversion Hnv as [Hnv0 [Hnv1 Hnv2]].
    subst ks.
    assert (
      Datatypes.length (map (fun k : string => "." ++ k) (map toString (nameSecondary n)))
        = Datatypes.length (nameSecondary n)
    ) as Hsame. {
      rewrite map_length.
      rewrite map_length.
      reflexivity.
    }
    rewrite Hsame.
    exact Hnv2.
  }
  assert (960 = 15 * 64) as Hn by intuition.
  rewrite Hn.
  pose proof (nameMapToStringForall n Hnv) as H1.
  pose proof (nameMapDottedForall _ H1) as H2.
  pose proof (foldAppendLength _ _ H2) as H3.
  subst ks.

  remember (
    (map (fun k : string => "." ++ k) (map toString (nameSecondary n)))
  ) as HM0.
  remember (
    (fold_left append HM0 "")
  ) as HF.
  apply (
    Nat.le_trans (length HF) (Datatypes.length HM0 * 64) (15 * 64) H3
  ).
  apply Nat.mul_le_mono_pos_r.
  intuition.
  exact H0.
Qed.

(** A valid name is always <= 1024 characters. *)
Theorem nameSize : forall (n : name),
  nameValid n -> length (toString n) <= 1024.
Proof.
  intros n Hv.
  unfold toString.
  unfold toStringName.
  unfold tsName.

  pose proof (nameSizeSecondary n Hv) as H0.
  remember (
    fold_left append (map (fun k : string => "." ++ k) (map toString (nameSecondary n))) ""
  ) as HF.

  inversion Hv as [Hv0 [Hv1 Hv2]].
  pose proof (toStringSegmentPrimaryLength (namePrimary n) Hv0) as H1.
  rewrite stringAppendListLength.
  assert (1024 = 64 + 960) as Hn by intuition.
  rewrite Hn.
  apply Nat.add_le_mono.
  exact H1.
  exact H0.
Qed.

Local Open Scope char_scope.

Example name0 : name := 
  MakeName (MakeSegmentPrimary "c" ["o";"m"]) [].

Lemma name0ts : toString name0 = "com"%string.
Proof. reflexivity. Qed.

Example name1 : name := 
  MakeName
    (MakeSegmentPrimary "c" ["o";"m"])
    [(MakeSegmentSecondary "i" ["o";"7";"m"]);
     (MakeSegmentSecondary "l" ["a";"n";"a";"r";"k"])].

Lemma name1ts : toString name1 = "com.io7m.lanark"%string.
Proof. reflexivity. Qed.

