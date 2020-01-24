import _ from 'lodash';

export class LinkUpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'link-update', false);
    if (data) {
      this.add(data, state);
      this.comments(data, state);
      this.commentAdd(data, state);
      this.commentPage(data, state);
      this.page(data, state);
    }
  }

  add(json, state) {
    // pin ok'd link POSTs to top of page0
    let data = _.get(json, 'add', false);
    if (data) {
    let path = Object.keys(data)[0];
    let tempArray = state.links[path].page0;
    tempArray.unshift(data[path]);
    state.links[path].page0 = tempArray;
    }
  }

  comments(json, state) {
    let data = _.get(json, 'comments', false);
    if (data) {
      let path = data.path;
      let page = "page" + data.page;
      let index = data.index;
      let storage = state.links[path][page][index];

      storage.comments = {};
      storage.comments["page0"] = data.data.page;
      storage.comments["total-items"] = data.data["total-items"];
      storage.comments["total-pages"] = data.data["total-pages"];

      state.links[path][page][index] = storage;
    }
  }

  commentAdd(json, state) {
    let data = _.get(json, 'commentAdd', false);
    if (data) {
      let path = data.path;
      let page = "page" + data.page;
      let index = data.index;

      let ship = window.ship;
      let time = data.time;
      let udon = data.udon;
      let tempObj = {
        'ship': ship,
        'time': time,
        'udon': udon
      }
      let tempArray = state.links[path][page][index].comments.page;
      tempArray.unshift(tempObj);
      state.links[path][page][index].comments.page = tempArray;
    }
  }

  commentPage(json, state) {
    let data = _.get(json, 'commentPage', false);
    if (data) {
      let path = data.path;
      let linkPage = "page" + data.linkPage;
      let linkIndex = data.index;
      let commentPage = "page" + data.comPageNo;

      if (!state.links[path]) {
        return false;
      }

      state.links[path][linkPage][linkIndex].comments[commentPage] = data.data;
    }
  }

  page(json, state) {
    let data = _.get(json, 'page', false);
    if (data) {
      let path = Object.keys(data)[0];
      let page = "page" + data[path].page;
      if (!state.links[path]) {
        state.links[path] = {};
      }
      state.links[path][page] = data[path].links;
    }
  }
}