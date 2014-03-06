#include "p/raft.capnp.h"
/* AUTO GENERATED - DO NOT EDIT */
static const capn_text capn_val0 = {0,""};

Raft_ptr new_Raft(struct capn_segment *s) {
	Raft_ptr p;
	p.p = capn_new_struct(s, 0, 0);
	return p;
}
Raft_list new_Raft_list(struct capn_segment *s, int len) {
	Raft_list p;
	p.p = capn_new_list(s, len, 0, 0);
	return p;
}
void read_Raft(struct Raft *s, Raft_ptr p) {
	capn_resolve(&p.p);
}
void write_Raft(const struct Raft *s, Raft_ptr p) {
	capn_resolve(&p.p);
}
void get_Raft(struct Raft *s, Raft_list l, int i) {
	Raft_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft(s, p);
}
void set_Raft(const struct Raft *s, Raft_list l, int i) {
	Raft_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft(s, p);
}

Raft_Comd_ptr new_Raft_Comd(struct capn_segment *s) {
	Raft_Comd_ptr p;
	p.p = capn_new_struct(s, 8, 1);
	return p;
}
Raft_Comd_list new_Raft_Comd_list(struct capn_segment *s, int len) {
	Raft_Comd_list p;
	p.p = capn_new_list(s, len, 8, 1);
	return p;
}
void read_Raft_Comd(struct Raft_Comd *s, Raft_Comd_ptr p) {
	capn_resolve(&p.p);
	s->typ = (enum Raft_Comd_Type) capn_read16(p.p, 0);
	s->bob = capn_get_data(p.p, 0);
}
void write_Raft_Comd(const struct Raft_Comd *s, Raft_Comd_ptr p) {
	capn_resolve(&p.p);
	capn_write16(p.p, 0, (uint16_t) s->typ);
	capn_setp(p.p, 0, s->bob.p);
}
void get_Raft_Comd(struct Raft_Comd *s, Raft_Comd_list l, int i) {
	Raft_Comd_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Comd(s, p);
}
void set_Raft_Comd(const struct Raft_Comd *s, Raft_Comd_list l, int i) {
	Raft_Comd_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Comd(s, p);
}

Raft_Rent_ptr new_Raft_Rent(struct capn_segment *s) {
	Raft_Rent_ptr p;
	p.p = capn_new_struct(s, 8, 1);
	return p;
}
Raft_Rent_list new_Raft_Rent_list(struct capn_segment *s, int len) {
	Raft_Rent_list p;
	p.p = capn_new_list(s, len, 8, 1);
	return p;
}
void read_Raft_Rent(struct Raft_Rent *s, Raft_Rent_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read32(p.p, 0);
	s->com.p = capn_getp(p.p, 0, 0);
}
void write_Raft_Rent(const struct Raft_Rent *s, Raft_Rent_ptr p) {
	capn_resolve(&p.p);
	capn_write32(p.p, 0, s->tem);
	capn_setp(p.p, 0, s->com.p);
}
void get_Raft_Rent(struct Raft_Rent *s, Raft_Rent_list l, int i) {
	Raft_Rent_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Rent(s, p);
}
void set_Raft_Rent(const struct Raft_Rent *s, Raft_Rent_list l, int i) {
	Raft_Rent_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Rent(s, p);
}

Raft_Rest_ptr new_Raft_Rest(struct capn_segment *s) {
	Raft_Rest_ptr p;
	p.p = capn_new_struct(s, 32, 2);
	return p;
}
Raft_Rest_list new_Raft_Rest_list(struct capn_segment *s, int len) {
	Raft_Rest_list p;
	p.p = capn_new_list(s, len, 32, 2);
	return p;
}
void read_Raft_Rest(struct Raft_Rest *s, Raft_Rest_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read32(p.p, 0);
	s->cid = capn_get_text(p.p, 0, capn_val0);
	s->lai = capn_read64(p.p, 8);
	s->lat = capn_read32(p.p, 4);
	s->which = (enum Raft_Rest_which) capn_read16(p.p, 16);
	switch (s->which) {
	case Raft_Rest_apen:
		s->apen.ent.p = capn_getp(p.p, 1, 0);
		s->apen.cit = capn_read64(p.p, 24);
		break;
	default:
		break;
	}
}
void write_Raft_Rest(const struct Raft_Rest *s, Raft_Rest_ptr p) {
	capn_resolve(&p.p);
	capn_write32(p.p, 0, s->tem);
	capn_set_text(p.p, 0, s->cid);
	capn_write64(p.p, 8, s->lai);
	capn_write32(p.p, 4, s->lat);
	capn_write16(p.p, 16, s->which);
	switch (s->which) {
	case Raft_Rest_apen:
		capn_setp(p.p, 1, s->apen.ent.p);
		capn_write64(p.p, 24, s->apen.cit);
		break;
	default:
		break;
	}
}
void get_Raft_Rest(struct Raft_Rest *s, Raft_Rest_list l, int i) {
	Raft_Rest_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Rest(s, p);
}
void set_Raft_Rest(const struct Raft_Rest *s, Raft_Rest_list l, int i) {
	Raft_Rest_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Rest(s, p);
}

Raft_Rasp_ptr new_Raft_Rasp(struct capn_segment *s) {
	Raft_Rasp_ptr p;
	p.p = capn_new_struct(s, 16, 0);
	return p;
}
Raft_Rasp_list new_Raft_Rasp_list(struct capn_segment *s, int len) {
	Raft_Rasp_list p;
	p.p = capn_new_list(s, len, 16, 0);
	return p;
}
void read_Raft_Rasp(struct Raft_Rasp *s, Raft_Rasp_ptr p) {
	capn_resolve(&p.p);
	s->tem = capn_read64(p.p, 0);
	s->suc = (capn_read8(p.p, 8) & 1) != 0;
}
void write_Raft_Rasp(const struct Raft_Rasp *s, Raft_Rasp_ptr p) {
	capn_resolve(&p.p);
	capn_write64(p.p, 0, s->tem);
	capn_write1(p.p, 64, s->suc != 0);
}
void get_Raft_Rasp(struct Raft_Rasp *s, Raft_Rasp_list l, int i) {
	Raft_Rasp_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Rasp(s, p);
}
void set_Raft_Rasp(const struct Raft_Rasp *s, Raft_Rasp_list l, int i) {
	Raft_Rasp_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Rasp(s, p);
}

Raft_Rmsg_ptr new_Raft_Rmsg(struct capn_segment *s) {
	Raft_Rmsg_ptr p;
	p.p = capn_new_struct(s, 8, 1);
	return p;
}
Raft_Rmsg_list new_Raft_Rmsg_list(struct capn_segment *s, int len) {
	Raft_Rmsg_list p;
	p.p = capn_new_list(s, len, 8, 1);
	return p;
}
void read_Raft_Rmsg(struct Raft_Rmsg *s, Raft_Rmsg_ptr p) {
	capn_resolve(&p.p);
	s->which = (enum Raft_Rmsg_which) capn_read16(p.p, 0);
	switch (s->which) {
	case Raft_Rmsg_rest:
	case Raft_Rmsg_rasp:
		s->rasp.p = capn_getp(p.p, 0, 0);
		break;
	default:
		break;
	}
}
void write_Raft_Rmsg(const struct Raft_Rmsg *s, Raft_Rmsg_ptr p) {
	capn_resolve(&p.p);
	capn_write16(p.p, 0, s->which);
	switch (s->which) {
	case Raft_Rmsg_rest:
	case Raft_Rmsg_rasp:
		capn_setp(p.p, 0, s->rasp.p);
		break;
	default:
		break;
	}
}
void get_Raft_Rmsg(struct Raft_Rmsg *s, Raft_Rmsg_list l, int i) {
	Raft_Rmsg_ptr p;
	p.p = capn_getp(l.p, i, 0);
	read_Raft_Rmsg(s, p);
}
void set_Raft_Rmsg(const struct Raft_Rmsg *s, Raft_Rmsg_list l, int i) {
	Raft_Rmsg_ptr p;
	p.p = capn_getp(l.p, i, 0);
	write_Raft_Rmsg(s, p);
}
