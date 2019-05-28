export class CirclesReducer {
  reduce(reports, store) {
    reports.forEach((rep) => {
      switch (rep.type) {
        case "circles":
          if (rep.data.add) {
            store.circles = [...store.circles, rep.data.cir]
          } else {
            store.circles = rep.data
          }
          break;

        case "landscape.prize":
          store.circles = [...store.circles, rep.data["circles-our"]];
          break;
      }
    });
  }
}
