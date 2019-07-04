#include "all.h"

#define TRUE 1
#define FALSE 0


/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y, c3n);
}

// malloc space and fill with a random string of length len_w
//
static c3_y * util_rand_string(c3_w len_w)
{
  c3_y * choice_y = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  c3_w choice_len_w = c3_sizet_to_w(strlen( choice_y));

  c3_y * out_y = malloc(len_w + 1);

  c3_w i_w;
  for (i_w = 0; i_w < len_w; i_w ++){
    out_y[i_w] = choice_y[ (c3_w) rand() % choice_len_w ];
  }
  out_y[i_w] = 0;

  return(out_y);
}

static void
_test_noun_bits_helper(u3_noun a, int direct_o, int indirect_o, int indirect_atom_o, int cell_o)
{
  c3_w verbose_w = 0;
  if (verbose_w){
    printf("=========== %lx\n", (uint64_t) a);
    printf("    63 bit  %lx\n", (uint64_t) a & ( ((c3_d) 1) << 63));
    printf("    62 bit  %lx\n", (uint64_t) a & ( ((c3_d) 1) << 62));
    printf("    dir     %x\n", u3a_is_direct_b(a));
    printf("    ind     %x\n", u3a_is_indirect_b(a));
    printf("  i cell    %x\n", u3a_is_cell_b(a));
    printf("  i atom    %x\n", u3a_is_indirect_atom_b(a));
  }

  // level 1 - direct testing of flags (boolean)

  if ( direct_o != u3a_is_direct_b(a) ) {
    printf("*** _test_noun_bits_one() fail: u3a_is_direct\r\n");
  }

  if ( indirect_o != u3a_is_indirect_b(a) ) {
    printf("*** fail-2 u3a_is_indirect %d\r\n", u3a_is_indirect_b(a));
  }

  if ( cell_o != u3a_is_cell_b(a) ) {
    printf("*** fail-4 u3a_is_cell\r\n");
  }

  if ( indirect_atom_o != u3a_is_indirect_atom_b(a) ) {
    printf("*** fail-3 u3a_is_indirect_atom\r\n");
  }


}
static void
_test_noun_bits_set()
{
  u3_noun a = 1;

  // flip indirect bit on

  a = u3a_to_indirect(a);
  if ( !  u3a_is_indirect_b(a) ) {
    printf("*** fail-5a turn indirect bit on\r\n");
  }

  if ( u3a_is_cell_b(a) ) {
    printf("*** fail-5b turn indirect bit on\r\n");
  }

  if (! u3a_is_indirect_atom_b(a)) {
    printf("*** fail-5c turn indirect bit on\r\n");
  }

  // flip all bits off
  a = u3a_to_off(a);

  if ( u3a_is_indirect_b(a) ) {
    printf("*** fail-6 turn indirect bit off\r\n");
  }

  if ( u3a_is_cell_b(a) ) {
    printf("*** fail-5b turn indirect bit on\r\n");
  }

  if ( u3a_is_indirect_atom_b(a)) {
    printf("*** fail-5c turn indirect bit on\r\n");
  }

  // flip indirect & cell bit on

  a = u3a_to_indirect_cell(a);

  if ( !  u3a_is_indirect_b(a) ) {
    printf("*** fail-5d turn indirect bit on\r\n");
  }

  if ( ! u3a_is_cell_b(a) ) {
    printf("*** fail-5e turn indirect bit on\r\n");
  }

  if (u3a_is_indirect_atom_b(a)) {
    printf("*** fail-5f turn indirect bit on\r\n");
  }


}

// flip
static void
_test_noun_bits_read()
{

  u3_noun a = ((u3_noun) 0x1);                                         // no bits set -> direct

  u3_noun b = ((u3_noun) 0x2) | u3_noun_indirectbit;                   // indirect, not a cell

  u3_noun c = ((u3_noun) 0x3) | u3_noun_indirectbit | u3_noun_cellbit; // indirect, a cell



                  // direct    indirect  indirect-atom   indirect-cell
                  //----------------------------------------

  _test_noun_bits_helper(a, TRUE,          FALSE,      FALSE,            FALSE);

  _test_noun_bits_helper(b, FALSE,         TRUE,       TRUE,             FALSE);

  _test_noun_bits_helper(c, FALSE,         TRUE,       FALSE,            TRUE);
}


// insert and retrieve bytes in a noun
//    u3_i_bytes() <--> u3r_bytes
static void
_test_imprison()
{

  c3_y * input_y =  "abcdefghij";
  c3_w out_len_w = 300;
  c3_y * output_y = malloc(out_len_w);
  u3_noun a;

  // size 1, direct
  a = u3i_bytes(1, input_y);
  bzero(output_y, out_len_w);
  u3r_bytes(0, 1, output_y, a);
  if (0 != strcmp(output_y, "a")) {
    printf("*** _test_imprison: fail-1\n");
  }

  // size 2, direct
  a = u3i_bytes(2, input_y);
  bzero(output_y, out_len_w);
  u3r_bytes(0, 2, output_y, a);
  if (0 != strcmp(output_y, "ab")) {
    printf("*** _test_imprison: fail-2\n");
  }

  // size 6, direct (taken from an actual issue)
  {
    c3_y data_y[] = { 0x1, 0x1f, 0x8e, 0x2d, 0x2c, 0x2f };
    a = u3i_bytes(6, data_y);
    bzero(output_y, out_len_w);
    u3r_bytes(0, 6, output_y, a);
    int ret;
    ret = memcmp(output_y, data_y, 6);
    if (0 != ret) {
      printf("*** _test_imprison: fail-2.5 %x\n", ret);
      printf("    %x %x %x %x %x %x\n", output_y[0], output_y[1], output_y[2], output_y[3], output_y[4], output_y[5]);
    }
  }

  // size 8, direct
  a = u3i_bytes(8, input_y);
  bzero(output_y, out_len_w);
  u3r_bytes(0, 8, output_y, a);
  if (0 != strcmp(output_y, "abcdefgh")) {
    printf("*** _test_imprison: fail-3\n");
  }

  // size 10, indirect
  a = u3i_bytes(10, input_y);
  bzero(output_y, out_len_w);
  u3r_bytes(0, 10, output_y, a);
  if (0 != strcmp(output_y, "abcdefghij")) {
    printf("*** _test_imprison: fail-4\n");
  }

  // size 200, indirect
  c3_y * rand_y = util_rand_string(200);
  a = u3i_bytes(200, rand_y);
  bzero(output_y, out_len_w);
  u3r_bytes(0, 200, output_y, a);
  if (0 != strcmp(output_y, rand_y)) {
    printf("*** _test_imprison: fail-5\n");
  }
  free(rand_y);

  free(output_y);

}


// test u3i_cell(), u3h(), u3t()
//
static void
_test_cells()
{

  c3_w verbose_w = 0;

  // very simple cell
  {

    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = u3i_cell(a, b);

    u3_noun a2 = u3h(c);
    if (a2 != a){
      printf("*** _test_cells: fail-1\n");
    }

    u3_noun b2 = u3t(c);
    if (b2 != b){
      printf("*** _test_cells: fail-2\n");
    }
  }

  // very simple cell with indirect atoms
  {

    c3_w out_len_w = 200;
    c3_y * rand_a = util_rand_string(out_len_w);
    c3_y * rand_b = util_rand_string(out_len_w);

    u3_noun a = u3i_bytes(200, rand_a);
    u3_noun b = u3i_bytes(200, rand_b);
    u3_noun c = u3i_cell(a, b);

    if (verbose_w){
      printf("a = %x\n", a);
      printf("b = %x\n", b);
      printf("c = %x\n", c);
      printf("a_rand = %s\n", rand_a);
      printf("b_rand = %s\n", rand_b);
    }

    u3_noun a2 = u3h(c);
    c3_y * output_y = malloc(out_len_w + 1);
    bzero(output_y, out_len_w + 1);
    u3r_bytes(0, out_len_w, output_y, a);

    if (verbose_w){
      printf("a_rand_out = %s\n", output_y);
    }

    if (0 != strcmp(output_y, rand_a)) {
      printf("*** _test_imprison: fail-3\n");
    }

    u3_noun b2 = u3h(c);
    bzero(output_y, out_len_w + 1);
    u3r_bytes(0, out_len_w, output_y, b);

    if (verbose_w){
      printf("b_rand_out = %s\n", output_y);
    }

    if (0 != strcmp(output_y, rand_b)) {
      printf("*** _test_imprison: fail-4\n");
    }

    free(rand_a);
    free(rand_b);
  }

  // medium complicated cell
  //    q
  //   / \
  //  a1  r
  //     / \
  //    b2  s
  //       / \
  //      c3  d4
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    
    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_noun a2 = u3h(q);
    u3_noun r2 = u3t(q);
    if (a2 != a){
      printf("*** _test_cells: complicated a\n");
    }

    u3_noun b2 = u3h(r2);
    u3_noun s2 = u3t(r2);
    if (b2 != b){
      printf("*** _test_cells: complicated b\n");
    }


    u3_noun c2 = u3h(s2);
    u3_noun d2 = u3t(s2);
    if (c2 != c){
      printf("*** _test_cells: complicated c\n");
    }

    if (d2 != d){
      printf("*** _test_cells: complicated d\n");
    }

    a2 = 0;
    u3r_mean(q, 2, &a2, 0);
    if (a2 != a){
      printf("*** _test_cells: complicated (via u3r_mean) a\n");
    }
    
  }

  // trel
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   c
    
    // Produce the triple `[a b c]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;

    u3_noun trel = u3i_trel(a, b, c);

    u3_noun a2 = u3h(trel);
    u3_noun b2 = u3h(u3t(trel));
    u3_noun c2 = u3t(u3t(trel));

    if (a2 != a){
      printf("*** trel: 1 a\n");
    }
    if (b2 != b){
      printf("*** trel: 2 a\n");
    }
    if (c2 != c){
      printf("*** trel: 3 a\n");
    }

    
  }

  // qual
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   ?
    //        /  \
    //       c   d
    //        
    
    // Produce the triple `[a b c]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun qual = u3i_qual(a, b, c, d);

    u3_noun a2 = u3h(qual);
    u3_noun b2 = u3h(u3t(qual));
    u3_noun c2 = u3h(u3t(u3t(qual)));
    u3_noun d2 = u3t(u3t(u3t(qual)));

    if (a2 != a){
      printf("*** qual: 1 \n");
    }
    if (b2 != b){
      printf("*** qual: 2 \n");
    }
    if (c2 != c){
      printf("*** qual: 3 \n");
    }
    if (d2 != d){
      printf("*** qual: 4 \n");
    }
  }





}

static void
_test_cells_complex()
{ 
  // trel
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   c
    
    // Produce the triple `[a b c]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;

    u3_noun q = u3i_trel(a, b, c);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;

    u3x_trel(q, &a2, &b2, &c2);

    if (a2 != a){
      printf("*** _test_cells_complex: trel() 1 a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: trel() 2 a\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: trel() 3 a\n");
    }
  }

  // qual
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   z
    //        / \
    //       c   d
    
    // Produce the qual `[a b c d]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun z = u3i_cell(c, d);
    u3_noun q = u3i_trel(a, b, z);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;
    u3_noun d2 = 0;

    u3x_qual(q, &a2, &b2, &c2, &d2);

    if (a2 != a){
      printf("*** _test_cells_complex: qual() a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: qual() b\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: qual() c\n");
    }
    if (d2 != d){
      printf("*** _test_cells_complex: qual() d\n");
    }
  }

  // quil
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   z
    //        / \
    //       c   ?
    //          / \
    //         d   e
    
    // Produce `[a b c d e]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    u3_noun e = (u3_noun) 0x5;

    u3_noun z = u3i_trel(c, d, e);
    u3_noun q = u3i_trel(a, b, z);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;
    u3_noun d2 = 0;
    u3_noun e2 = 0;

    u3x_quil(q, &a2, &b2, &c2, &d2, &e2);

    if (a2 != a){
      printf("*** _test_cells_complex: quil() a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: quil() b\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: quil() c\n");
    }
    if (d2 != d){
      printf("*** _test_cells_complex: quil() d\n");
    }
    if (e2 != e){
      printf("*** _test_cells_complex: quil() e\n");
    }
  }

  // hext
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   z
    //        / \
    //       c   ?
    //          / \
    //         d   .
    //            / \
    //           e   f
    //
    // Produce `[a b c d e f]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    u3_noun e = (u3_noun) 0x5;
    u3_noun f = (u3_noun) 0x6;

    u3_noun z = u3i_trel(d, e, f);
    u3_noun q = u3i_qual(a, b, c, z);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;
    u3_noun d2 = 0;
    u3_noun e2 = 0;
    u3_noun f2 = 0;

    u3x_hext(q, &a2, &b2, &c2, &d2, &e2, &f2);

    if (a2 != a){
      printf("*** _test_cells_complex: hext() a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: hext() b\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: hext() c\n");
    }
    if (d2 != d){
      printf("*** _test_cells_complex: hext() d\n");
    }
    if (e2 != e){
      printf("*** _test_cells_complex: hext() e - e2 = %i\n", e2);
    }
    if (f2 != f){
      printf("*** _test_cells_complex: hext() f - f2 = %i\n", f2);
    }
  }

  
}


// test functions from imprison.h / retrieve.h
static void
_test_imprison_complex()
{

  // vint
  {
    u3_noun a = 1;

    u3_noun b= u3i_vint(a);
    if (2 != b){
      printf("*** vint 1\n");
    }

    u3_noun c= u3i_vint(b);
    if (3 != c){
      printf("*** vint 2\n");
    }

    a = 1ull << 50;
    b= u3i_vint(a);
    
    if ((a + 1) != b){
      printf("*** vint 3\n");
    }

  }
  

    // bytes
  {
    c3_y in_y[10] = { 10, 20, c3_y_MAX};
    u3_noun a = u3i_bytes(3, in_y);
    
    c3_w out_a = u3r_byte(0, a);
    if (10 != out_a ){
      printf("*** u3r_byte 1\n");
    }

    c3_w out_b = u3r_byte(1, a);
    if (20 != out_b ){
      printf("*** u3r_byte 2\n");
    }

    c3_w out_c = u3r_byte(2, a);
    if (c3_y_MAX != out_c ){
      printf("*** u3r_byte 3\n");
    }

    c3_y out_y[10];
    bzero((void *) out_y, 10 * sizeof(c3_y));
    u3r_bytes(0, 3, out_y, a);

    if (10 != out_y[0] ||
        20 != out_y[1] ||
        c3_y_MAX != out_y[2] ||
        0 != out_y[3]
        ){
      printf("*** u3r_byte 4\n");
    }
  }

  // words
  {
    c3_w in_w[10] = {10, 20, c3_w_MAX};
    u3_noun noun = u3i_words(3, in_w);
    

    c3_w out_a = u3r_word(0, noun);
    if (10 != out_a ){
      printf("*** u3r_word 1\n");
    }

    c3_w out_b = u3r_word(1, noun);
    if (20 != out_b ){
      printf("*** u3r_word 2\n");
    }

    c3_w out_c = u3r_word(2, noun); 
    if (c3_w_MAX != out_c ){
      printf("*** u3r_word 3\n");
    }

    c3_w out_w[10];
    bzero((void *) out_w, 10 * sizeof(c3_w));
    u3r_words(0, 3, out_w, noun);

    if (10 != out_w[0] ||
        20 != out_w[1] ||
        c3_w_MAX != out_w[2] ||
        0 != out_w[3]
        ){
      printf("*** u3r_word 4\n");
    }
  }

  // chubs
  {
    c3_d in_d[10] = {1, 2, c3_d_MAX};

    c3_d out_d[10];

    u3_noun a =  u3i_chubs(1, & in_d[0]);
    bzero(out_d, sizeof(c3_d) * 10);
    u3r_chubs(0, 1, out_d, a);
    if (1 != out_d[0] ){
      printf("*** u3r_chubs 1\n");
    }

    
    u3_noun b =  u3i_chubs(1, & in_d[1]);
    bzero(out_d, sizeof(c3_d) * 10);
    u3r_chubs(0, 1, out_d, b);
    if (2 != out_d[0] ){
      printf("*** u3r_chubs 2\n");
    }

    u3_noun c =  u3i_chubs(1, & in_d[2]);
    bzero(out_d, sizeof(c3_d) * 10);
    u3r_chubs(0, 1, out_d, c);
    if (c3_d_MAX != out_d[0] ){
      printf("*** u3r_chubs 3\n");
    }

    u3_noun d =  u3i_chubs(3, in_d);
    bzero(out_d, sizeof(c3_d) * 10);
    u3r_chubs(0, 3, out_d, d);
    if (1 != out_d[0] ){
      printf("*** u3r_chubs 4-a\n");
    }
    if (2 != out_d[1] ){
      printf("*** u3r_chubs 4-b\n");
    }
    if (c3_d_MAX != out_d[2] ){
      printf("*** u3r_chubs 4-c\n");
    }
    
  }
  
  // string
  {

    c3_c * in_c = "a";
    u3_noun noun = u3i_string(in_c);
    c3_c* out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    in_c = "ab";
    noun = u3i_string(in_c);
    out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    in_c = "abcd";
    noun = u3i_string(in_c);
    out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    in_c = "this is a test";
    noun = u3i_string(in_c);
    out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    
  }

  // tape
  {

    c3_c * in_c = "this is a test";
    u3_noun noun = u3i_tape(in_c);

    c3_c* out_c = u3r_tape(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_tape 1\n");
    }

    // tape stores each byte in the string as one atom in the tree
    u3_noun lent = u3qb_lent(noun);
    if (u3a_noun_to_w(lent) != strlen(in_c)){
      printf("*** u3r_tape 2\n");
    }
    
  }

  // edit
  {  

  //    q
  //   / \
  //  a1  r
  //     / \
  //    b2  s
  //       / \
  //      c3  d4

    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    
    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_noun axis = 2;
    u3_noun newval = 99;
    u3_noun hacked = u3i_edit(q, axis, newval);

    u3_noun read_1;
    u3r_mean(hacked, axis, &read_1, 0);

    if (newval != read_1){
      printf("*** u3i_edit 1\n");
    }
  }

  // molt
  {  

    //    q
    //   / \
    //  a1  r
    //     / \
    //    b2  s
    //       / \
    //      c3  d4

    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    
    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_noun axis_1 = 2;
    u3_noun newval_1 = 99;

    u3_noun axis_2 = 6;
    u3_noun newval_2 = 777;
    
    u3_noun hacked = u3i_molt(q, axis_1, newval_1, axis_2, newval_2, 0);

    u3_noun read_1;
    u3_noun read_2;
    u3r_mean(hacked, axis_1, &read_1, axis_2, &read_2, 0);

    if (newval_1 != read_1){
      printf("*** u3i_molt 1\n");
    }

    if (newval_2 != read_2){
      printf("*** u3i_molt 2\n");
    }
  }
}

// sing -  Yes iff (a) and (b) are the same noun.
void _test_sing()
{
  // direct noun
  //
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x1;
    u3_noun c = (u3_noun) 0x2;

    if (c3y != u3r_sing(a, a)) {
      printf("*** sing direct: 1 \n");
    }

    if (c3y != u3r_sing(a, b)) {
      printf("*** sing direct: 2 \n");
    }

    if (c3n != u3r_sing(a, c)) {
      printf("*** sing direct: 3 \n");
    }
  }

  // indirect
  //
  {
    c3_y * in_alpha_y =  "abcdefghijklmnopqrstuvwxyz";
    c3_y * in_numer_y =  "0123456789001234567890";

    
    u3_noun a = u3i_string(in_alpha_y);
    u3_noun b = u3i_string(in_alpha_y);
    u3_noun c = u3i_string(in_numer_y);

    if (c3y != u3r_sing(a, a)) {
      printf("*** sing indirect: 1 \n");
    }

    if (c3y != u3r_sing(a, b)) {
      printf("*** sing indirect: 2 \n");
    }

    if (c3n != u3r_sing(a, c)) {
      printf("*** sing indirect: \n");
    }
  }
    
}
  


// fing -  same copy of the same noun.
void _test_fing()
{
  // direct noun
  //
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x1;
    u3_noun c = (u3_noun) 0x2;

    if (c3y != u3r_fing(a, a)) {
      printf("*** fing direct: 1 \n");
    }

    if (c3y != u3r_fing(a, b)) {
      printf("*** fing direct: 2 \n");
    }

    if (c3n != u3r_fing(a, c)) {
      printf("*** fing direct: 3 \n");
    }
  }

  // indirect
  //
  {
    c3_y * in_alpha_y =  "abcdefghijklmnopqrstuvwxyz";
    c3_y * in_numer_y =  "0123456789001234567890";

    
    u3_noun a = u3i_string(in_alpha_y);
    u3_noun b = u3i_string(in_alpha_y);
    u3_noun c = u3i_string(in_numer_y);

    if (c3y != u3r_fing(a, a)) {
      printf("*** fing indirect: 1 \n");
    }

    if (c3n != u3r_fing(a, b)) {
      printf("*** fing indirect: 2 \n");
    }

    if (c3n != u3r_fing(a, c)) {
      printf("*** fing indirect: \n");
    }
  }
    
}

void _test_met()
{
  c3_w ret_w;
  u3_atom atom;

  // 1
   {
    atom = 1;
    
    ret_w = u3r_met(0, atom);
    if (1 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(4, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 2 = 0b10
   {
    atom = 2;
    
    ret_w = u3r_met(0, atom);
    if (2 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 8=0b1000
   {
    atom = 8;
    
    ret_w = u3r_met(0, atom);
    if (4 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 0xff = 255 =0b 1111 1111
   {
    atom = 0xff;
    
    ret_w = u3r_met(0, atom);
    if (8 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 0x100 = 256 =0b 0001 1111 1111
   {
    atom = 0x100;
    
    ret_w = u3r_met(0, atom);
    if (9 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (2 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 32 bit direct
  // 0x ff ff ff ff 
   {
    atom = 0xffffffffULL;
    
    ret_w = u3r_met(0, atom);
    if (32 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (4 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 4 words x 32 bits each = 128 bits = 16 bytes = 4 words = 2 doubles
  // 
  if (1) {
                     
    c3_w data_w[4] = { c3_w_MAX, c3_w_MAX, c3_w_MAX, c3_w_MAX };
    
    atom = u3i_words(4, data_w);

    
    ret_w = u3r_met(0, atom);
    if (128 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (16 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (4 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (2 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 4 words (top word is '1' )
  // 
   {
                     
    c3_w data_w[4] = { c3_w_MAX, c3_w_MAX, c3_w_MAX, 1 };
    
    atom = u3i_words(4, data_w);

    
    ret_w = u3r_met(0, atom);
    if (97 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (13 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (4 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }
    
    ret_w = u3r_met(6, atom);
    if (2 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  

}

void _test_cue_jam()
{
  
  // the boot msg from the worker
  {
    u3_noun dat = u3_nul;
    u3_noun in_1 = u3nc(c3__play, dat);
    u3_atom jam_1 = u3ke_jam(in_1);

    u3_noun out_1 = u3ke_cue(jam_1);
    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);

    if (c3__play != head_out){
      printf("*** cue_jam 0 out head \n");
    }

    if (u3_nul != tail_out){
      printf("*** cue_jam 0 out tail \n");
    }

  }

  // the boot msg from the worker, again, but this time torn apart into bytes and rebuilt
  {
    u3_noun dat = u3_nul;
    u3_noun in_1 = u3nc(c3__play, dat);
    u3_atom jam_1 = u3ke_jam(in_1);

    c3_y buf_y[1024];
    bzero(buf_y, 1024);
    c3_w len_w = u3r_met(3, jam_1);

    u3r_bytes(0,       // start byte
              len_w,   // len
              buf_y,   // buffer
              jam_1 ); // input noun

    /// zip ....zap ... communicate between serf and king
    
    u3_noun jam_2 = u3i_bytes(len_w, buf_y);

    if (jam_1 != jam_2){
      printf("*** error in 6 byte message ; is u3ke_jam() building an indirect atom?? \n");
    }

    
    u3_noun out_1 = u3ke_cue(jam_2);
    
    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);

    if (c3__play != head_out){
      printf("*** cue_jam 0 out head \n");
    }

    if (u3_nul != tail_out){
      printf("*** cue_jam 0 out tail \n");
    }

  }
  
  return;
  
  // 1
  {
    
    u3_atom in_1 = 1;
    u3_atom jam_1 = u3ke_jam(in_1);

    if (12 != jam_1){
      printf("*** cue_jam 1a \n");
    }

    u3_noun out_1 = u3ke_cue(jam_1);

    if (1 != out_1){
      printf("*** cue_jam 1b \n");
    }

  }

  // [ 1 1 ] 
  {

    u3_noun in_1 = u3i_cell(1, 1);
    u3_atom jam_1 = u3ke_jam(in_1);

    if (817 != jam_1){
      printf("*** cue_jam 2 in \n");
    }

    u3_noun out_1 = u3ke_cue(jam_1);

    
    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);
    
    if (1 != head_out){
      printf("*** cue_jam 2 out head \n");
    }

    if (1 != tail_out){
      printf("*** cue_jam 2 out tail \n");
    }

  }

  // [ 1 2 ] 
  {

    u3_noun in_1 = u3i_cell(1, 2);
    u3_atom jam_1 = u3ke_jam(in_1);

    if (4657 != jam_1){
      printf("*** cue_jam 2 in \n");
    }

    u3_noun out_1 = u3ke_cue(jam_1);

    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);
    
    if (1 != head_out){
      printf("*** cue_jam 2 out head \n");
    }

    if (2 != tail_out){
      printf("*** cue_jam 2 out tail \n");
    }
  }


  
  // medium complicated cell
  //    q
  //   / \
  //  a1  r
  //     / \
  //    b2  s
  //       / \
  //      c3  d4
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    
    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_atom jam_1 = u3ke_jam(q);

    u3_noun out_1 = u3ke_cue(jam_1);

    
    u3_noun a2 = u3h(out_1);
    u3_noun r2 = u3t(out_1);
    if (a2 != a){
      printf("*** _cue_jam: complicated a\n");
    }

    u3_noun b2 = u3h(r2);
    u3_noun s2 = u3t(r2);
    if (b2 != b){
      printf("*** _cue_jam: complicated b\n");
    }


    u3_noun c2 = u3h(s2);
    u3_noun d2 = u3t(s2);
    if (c2 != c){
      printf("*** _cue_jam: complicated c\n");
    }

    if (d2 != d){
      printf("*** _cue_jam: complicated d\n");
    }

  }

}

void  _test_into_outa()
{
  printf("we should really test into() / outa() here\n");
  
}

void _test_cue_pil()
{
  extern c3_w u3_Ivory_length_w;
  extern c3_y u3_Ivory_pill_y[];

  u3_noun pil = u3i_bytes(u3_Ivory_length_w, u3_Ivory_pill_y);
  
  u3_noun arv = u3ke_cue(pil);   // 'pil' is a simple bignum ; turn it into a tree 'arv' by cue-ing
}


static void
_test_u3r_at()
{

  c3_w a_w;
  a_w = u3x_dep(0);
  if (0xffffffff != a_w) {  printf("*** u3x_dep() \n"); }
  
  a_w = u3x_dep(1);
  if (0 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b10);
  if (1 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b11);
  if (1 != a_w) {  printf("*** u3x_dep() \n"); }
  
  a_w = u3x_dep(0b100);
  if (2 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b110);
  if (2 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b111);
  if (2 != a_w) {  printf("*** u3x_dep() \n"); }
  
  a_w = u3x_dep( ((c3_w) (((c3_d) 1  << 32) - 1)) );
  if (31 != a_w) {  printf("*** u3x_dep() \n"); }


#if 0
  a_w = u3x_dep_d(0);
  a_w = u3x_dep_d(1);
  a_w = u3x_dep_d(0b10);
  a_w = u3x_dep_d(0b11);
  a_w = u3x_dep_d(0b100);
  a_w = u3x_dep_d( ((c3_w) (((c3_d) 1  << 32) - 1)) );
  a_w = u3x_dep_d( ((c3_w) (((c3_d) 1  << 33) - 1)) );
  a_w = u3x_dep_d( ((c3_d) (((c3_d) 1  << 64) - 1)) );
#endif
  
  u3_weak ret;

  // addr 1 in atom == atom value
  u3_noun tree = 1;
  ret = u3r_at( 1, tree);
  if (1 != ret) {  printf("*** u3r_at()\n"); }

  // addr 1 in atom == atom value
  tree = 2;
  ret = u3r_at( 1, tree);
  if (2 != ret) {  printf("*** u3r_at \n"); }

  // illegal
  ret = u3r_at( 2, tree);
  if (u3_none != ret) {  printf("*** u3r_at \n"); }


  // simple tree [ 1 2]
  tree = u3i_cell(10, 20);
  ret = u3r_at( 1, tree);
  if (tree != ret) {  printf("*** u3r_at \n"); }
  
  ret = u3r_at( 2, tree);
  if (10 != ret) {  printf("*** u3r_at \n"); }
  ret = u3r_at( 3, tree);
  if (20 != ret) {  printf("*** u3r_at \n"); }

  // simple tree [ 1 <BIGNUM>]
  c3_w in_w[10] = {10, 20, c3_w_MAX};
  u3_noun bignum = u3i_words(3, in_w);

  tree = u3i_cell(99, bignum);
  ret = u3r_at( 2, tree);
  if (99 != ret) {  printf("*** u3r_at \n"); }
  ret = u3r_at( 3, tree);
  if (bignum != ret) {  printf("*** u3r_at \n"); }

  
}

void  _test_u3r_chop()
{
  c3_w   dst_w = 0;


  // read 1 bit bloq
  {
    // read 1 bit from pos=0 (far right)
    u3_atom src = 0b11011;

    c3_g bloqsize_g = 0;

    
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0x1 != dst_w) {  printf("*** test_u3r_chop \n"); }


    // read 1 bit from pos=1
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             1,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0x1 != dst_w) {  printf("*** test_u3r_chop 2\n"); }

    // read 1 bit from pos=2
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             2,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0x0 != dst_w) {  printf("*** test_u3r_chop 3\n"); }

    // read 4 x 1 bit bloq from pos=0
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             4,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b1011 != dst_w) {  printf("*** test_u3r_chop 4\n"); }


    // read 1 x 1 bit bloq from pos=0 into offset 1
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             4,   // count of bloqs
             1,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b10110 != dst_w) {  printf("*** test_u3r_chop 5\n"); }


    
  }

  // read 2 bit bloq
  {
    u3_atom src = 0b11011;

    c3_g bloqsize_g = 1;

    // read 2 bit from pos=0 (far right)
    
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b11 != dst_w) {  printf("*** test_u3r_chop 2.1\n"); }


    // read 2 bit from pos=1 (1 bloq over )
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             1,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b10 != dst_w) {  printf("*** test_u3r_chop 2.2\n"); }

    // read 2 bit from pos=2 (2 bloq over)
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             2,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b01 != dst_w) {  printf("*** test_u3r_chop 2.3\n"); }

  }

  // read 8 bit bloq
  {
    u3_atom src = 0b11011;

    c3_g bloqsize_g = 3; // 2^3 = 8 bits

    // pos=0 (far right)
    
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b11011 != dst_w) {  printf("*** test_u3r_chop 8.1\n"); }
  }

  // read 1,8,16 bit bloqs from an indirect atom
  {
    // build an indirect noun 'src'
    
    c3_y * input_y =  "abcdefghij";
    u3_noun src = u3i_bytes(10, input_y);

    
    c3_g bloqsize_g = 0;  // 2^0 = 1 bit

    // 1 x 1 bit pos=0 (far right)
    //                - because data is stored LSB first, this pulls the lowest bit of the 'a' out
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b1 != dst_w) {  printf("*** test_u3r_chop indirect.1\n"); }

    // 8 x 1 bit pos=0 (far right), because data is stored LSB first, this pulls the 'a' out
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             8,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b1100001 != dst_w) {  printf("*** test_u3r_chop indirect.2\n"); }

    // 1 x 1 byte = 8 bit, pos=0 (far right), because data is stored LSB first, this pulls the 'a' out
    bloqsize_g = 3;  // 2^3 = 1 byte
    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b1100001 != dst_w) {  printf("*** test_u3r_chop indirect.3\n"); }

    
    // 1 x 16 bit bloq, pos = 0
    bloqsize_g = 4;  // 2^4 = 2 bytes

    dst_w = 0;
    u3r_chop(bloqsize_g,   /// bloq size
             0,   // start index
             1,   // count of bloqs
             0,   // end index
             & dst_w,   // where bytes go to
             src);     // where bytes come from

    if (0b0110001001100001 != dst_w) {  printf("*** test_u3r_chop indirect.4\n"); }

    
  }


  // read lots of bits from a direct noun which holds 64 bits of data
  // makes sure that we handle top 32 / bottom 32 correctly
  {
    // build an indirect noun 'src'
    
    c3_y input_y[8] =  { 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7 };
    u3_noun src = u3i_bytes(8, input_y);
    
    c3_g bloqsize_g = 0;  // 2^0 = 1 bit

    c3_w dst_w[2];
    bzero(dst_w, 2 * sizeof(c3_w));
          
    u3r_chop(bloqsize_g,   /// bloq size
             0,       // start index
             63,      // count of bloqs
             0,       // offset on out index
             dst_w,   // where bytes go to
             src);    // where bytes come from

  }

  // as above (read lots of bits from a direct noun which holds 64 bits of data
  // makes sure that we handle top 32 / bottom 32 correctly)
  // but with a bit more nuance
  {


    //                   least significant                    most
    c3_y input_y[8] =  { 0x0, 0x0, 0x0, 0xaa, 0xff, 0x0, 0x0, 0x0 };
    u3_noun src = u3i_bytes(8, input_y);
    
    c3_g bloqsize_g = 0;  // 2^0 = 1 bit

    c3_w dst_w = 0;
          
    u3r_chop(bloqsize_g,   /// bloq size
             24,       // start index
             16,      // count of bloqs
             0,       // offset on out index
             & dst_w,   // where bytes go to
             src);    // where bytes come from

    if (0b1111111110101010 != dst_w) {  printf("*** test_u3r_chop indirect. 6\n"); }

  }

}


void _n_push(c3_ys mov, c3_ys off, u3_noun a);
u3_noun * _n_peek(c3_ys off);
u3_noun* _n_peet(c3_ys mov, c3_ys off);
void _n_pop(c3_ys mov);
u3_noun _n_pep(c3_ys mov, c3_ys off);
void _n_toss(c3_ys mov, c3_ys off);
u3_noun* _n_swap(c3_ys mov, c3_ys off);





void _test_nvm_stack_inner(c3_ys mov,  c3_ys off)
{
  u3_noun * peek;
  
  // push 1, peek, pop
  if(1)  {
    _n_push(mov, off, 0x1122334455667788);
    peek = _n_peek(off);
    if (0x1122334455667788 != *peek) {  printf("*** test_nvm_stack 1\n"); }
    _n_pop(mov);
  }
  
  // push 2, peek, pop, peek, pop
  if(1)  {
    _n_push(mov, off, 88);
    _n_push(mov, off, 99);
    peek = _n_peek(off);
    if (99 != *peek) {  printf("*** test_nvm_stack 2\n"); }
    _n_pop(mov);
    peek = _n_peek(off);
    if (88 != *peek) {  printf("*** test_nvm_stack 3\n"); }
    _n_pop(mov);
  }

  // 100 x (push, peek, pop)
  {
    int ii;
    for (ii=0; ii <= 100; ii++){
      _n_push(mov, off, (u3_noun) ii);
    }
    for (ii=100; ii >= 0; ii--){
      peek = _n_peek(off);
      if (ii != *peek) {  printf("*** test_nvm_stack 4\n"); } 
      _n_pop(mov);
    }
  }
  
  // peet()
  {
    _n_push(mov, off, 333);
    _n_push(mov, off, 444);

    peek = _n_peet(mov, off);
    if (333 != *peek) {  printf("*** test_nvm_stack 5\n"); }

    _n_push(mov, off, 555);
    peek = _n_peet(mov, off);
    if (444 != *peek) {  printf("*** test_nvm_stack 6\n"); }
  }

  // pep()
  {
    _n_push(mov, off, 777);
    u3_noun ret =    _n_pep(mov, off);
    if (777 != ret) {  printf("*** test_nvm_stack 7\n"); }


    _n_push(mov, off, 777);
    _n_push(mov, off, 888);
    ret =    _n_pep(mov, off);
    if (888 != ret) {  printf("*** test_nvm_stack 8\n"); }
    ret =    _n_pep(mov, off);
    if (777 != ret) {  printf("*** test_nvm_stack 9\n"); }

  }

  // toss
  {
    _n_push(mov, off, 2777);
    _n_push(mov, off, 3888);
    _n_toss(mov, off);
    u3_noun ret =    _n_pep(mov, off);
    if (2777 != ret) {  printf("*** test_nvm_stack 10\n"); }

  }

  // swap
  {
    _n_push(mov, off, 2002);
    _n_push(mov, off, 3003);
    _n_swap(mov, off);
    u3_noun ret =    _n_pep(mov, off);
    if (2002 != ret) {  printf("*** test_nvm_stack 11\n"); }
    ret =    _n_pep(mov, off);
    if (3003 != ret) {  printf("*** test_nvm_stack 12\n"); }

  }


    
}


u3_noun _test_nvm_stack_south(u3_noun arg)
{
  c3_ys mov = 2;
  c3_ys off = -2;
  
  _test_nvm_stack_inner(mov, off);

}

void _test_nvm_stack()
{
  // north road
  c3_ys mov = -2;
  c3_ys off = 0;
  _test_nvm_stack_inner(mov, off);

  // south road
  u3_noun ret =  u3m_soft(100,                     // c3_w   sec_w
                          & _test_nvm_stack_south, // u3_funk fun_f
                          0);                      // u3_noun arg


}


/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
   _setup();

   // allocate.h level 1
  _test_noun_bits_read();

  // allocate.h level 1a
  _test_noun_bits_set();


  // test the "extract bit slices from atom" function u3r_chop()
  _test_u3r_chop();
  

  // 'jam' = marshall / compress /serialize ; 'cue' = unpack
  _test_cue_jam();

  // test basic data into / out of nouns
  _test_imprison();

  // test more complicated data into / out of nouns
  _test_imprison_complex();


  // 'sing' = equality
  _test_sing();

  // 'fing' = equality
  _test_fing();

  // 'met' = measure / take size
  _test_met();


  // cue the entire ivory pill, to really exercise 'cue'
  _test_cue_pil();

  // take loom numbers and convert them into pointers to loom memory
  _test_into_outa();

  // build and inspect cells:  u3i_cell(), u3h(), u3t()
  _test_cells();

  // build cells with more complex methods
  _test_cells_complex();

  // inspect cells at arbitrary axis locations u3r_at() [ and utility function u3x_dep()  ]
  _test_u3r_at();

  // test the stack functions used inside the Nock Virtual Machine (growing in both directions: N and S)
  _test_nvm_stack();

  
  printf("all tests done\n");
  
  return 0;
}
