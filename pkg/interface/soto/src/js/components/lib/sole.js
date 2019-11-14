// See /lib/sole/hoon

const str = JSON.stringify;

export class Share {
  constructor(buf, ven, leg) {
    if (buf == null) { buf = ""; }
    this.buf = buf;
    if (ven == null) { ven = [0, 0]; }
    this.ven = ven;
    if (leg == null) { leg = []; }
    this.leg = leg;
  }

  abet() { 
    return { buf:this.buf, 
             leg:this.leg.slice(), 
             ven:this.ven.slice() 
           }; 
    }

  apply(ted){
    switch (false) {
      case 'nop' !== ted: return;
      case !ted.map: return ted.map(this.apply, this);
      default: switch (Object.keys(ted)[0]) {
        case 'set': return this.buf = ted.set;
        case 'del': return this.buf = this.buf.slice(0,ted.del) + this.buf.slice(ted.del + 1);
        case 'ins':
          var { at, cha } = ted.ins;
          return this.buf = this.buf.slice(0,at) + cha + this.buf.slice(at);
        default: throw `%sole-edit -lost.${str(ted)}`;
      }
    }
  }

  transmute(sin,dex){
    switch (false) {
      case (sin !== 'nop') && (dex !== 'nop'): return dex;
      case !sin.reduce:
        return sin.reduce(((dex,syn) => this.transmute(syn,dex)), dex);
      case !dex.map: return dex.map(dax => this.transmute(sin,dax));
      case dex.set === undefined: return dex;
      default: switch (Object.keys(sin)[0]) {
        case 'set': return 'nop';
        case 'del':
          if (sin.del === dex.del) { return 'nop'; }
          dex = { ...dex }
          switch (Object.keys(dex)[0]) {
            case 'del': if (sin.del < dex.del) {    dex.del--; } break;
            case 'ins': if (sin.del < dex.ins.at) { dex.ins.at--; } break;
          }
          return dex;
        case 'ins':
          dex = { ...dex };
          var {at,cha} = sin.ins;
          switch (Object.keys(dex)[0]) {
            case 'del': if (at < dex.del) { dex.del++; } break;
            case 'ins': if ((at < dex.ins.at) ||
                              ((at === dex.ins.at) && !(cha <= dex.ins.cha))) {
                dex.ins.at++;
              } break;
          }
          return dex;
        default: throw `%sole-edit -lost.${str(sin)}`;
      }
    }
  }

  commit(ted){
    this.ven[0]++;
    this.leg.push(ted);
    return this.apply(ted);
  }

  inverse(ted){
    switch (false) {
      case 'nop' !== ted: return ted;
      case !ted.map:
        return ted.map( tad=> { const res=this.inverse(tad); this.apply(tad); return res; }).reverse();
      default: switch (Object.keys(ted)[0]) {
        case 'set': return {set: this.buf};
        case 'ins': return {del: ted.ins};
        case 'del': return {ins: {at: ted.del, cha: this.buf[ted.del]}};
        default: throw `%sole-edit -lost.${str(ted)}`;
      }
    }
  }

  receive({ler,ted}){
    if (!(ler[1] === this.ven[1])) { 
      throw `-out-of-sync.[${str(ler)} ${str(this.ven)}]`;
    }
    this.leg = this.leg.slice((this.leg.length + ler[0]) - this.ven[0]); 
    const dat = this.transmute(this.leg, ted);
    this.ven[1]++; 
    this.apply(dat); 
    return dat;
  }

  remit() { 
    throw 'stub'; 
  }

  transmit(ted){
    const act = {ted, ler:[this.ven[1], this.ven[0]]};
    this.commit(ted);
    return act;
  }

  transceive({ler,ted}){
    const old = new Share(this.buf);
    const dat = this.receive({ler, ted});
    return old.inverse(dat);
  }

  transpose(ted,pos){
    if (pos === undefined) { 
      return this.transpose(this.leg, ted);
    } else { 
      let left;
    return ((left = (this.transmute(ted, {ins: {at: pos}})).ins) != null ? left : {at:0}).at; }
  }
};

export default Share