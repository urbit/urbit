import _ from 'lodash';

export class PublicReducer {
  reduce(reports, store) {
    reports.forEach((rep) => {
      if (rep.type == "public") {
        if (_.isArray(rep.data)) {
          rep.data.forEach((c) => {
            this.storeCircle(`~${rep.from.ship}`, c, store.public);
          })
        } else {
          if (rep.data.add) {
            this.storeCircle(`~${rep.from.ship}`, rep.data.cir, store.public);
          } else {
            this.removeCircle(`~${rep.from.ship}`, rep.data.cir, store.public);
          }
        }
      }
    });
  }
  storeCircle(ship, circle, storePublic) {
    if (!storePublic[ship]) {
      storePublic[ship] = [circle];
    } else {
      if (storePublic[ship].indexOf(circle) === -1) {
        storePublic[ship] = [...storePublic[ship], circle];
      }
    }
  }
  removeCircle(ship, circle, storePublic) {
    storePublic[ship] = storePublic[ship].filter((e) => e !== circle);
  }
}
