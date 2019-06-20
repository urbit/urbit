export class RumorReducer {
  reduce(json, state){
    if (json.collection) {
      this.reduceCollection(json.collection, state);
    }
    if (json.post) {
      this.reducePost(json, state);
    }
    if (json.comments) {
      this.reduceComments(json, state);
    }
    if (json.total) {
      this.reduceTotal(json, state);
    }
    if (json.remove) {
      this.reduceRemove(json.remove, state);
    }
  }

  reduceRemove(json, state) {
    if (json.who === window.ship) {
      if (json.post) {
        this.removePost(json, state);
        delete state.pubs[json.coll].posts[json.post]; 
      } else {
        let postIds = Object.keys(state.pubs[json.coll].posts);
        postIds.forEach((postId) => {
          this.removePost({
            who: json.who,
            coll: json.coll,
            post: postId,
          }, state);
        });
        delete state.pubs[json.coll];
      }
    } else {
      if (json.post) {
        this.removePost(json, state);
        delete state.subs[json.who][json.coll].posts[json.post]; 
      } else {
        let postIds = Object.keys(state.subs[json.who][json.coll].posts);
        postIds.forEach((postId) => {
          this.removePost({
            who: json.who,
            coll: json.coll,
            post: postId,
          }, state);
        });
        delete state.subs[json.who][json.coll];
      }
    }
  }

  removePost(json, state) {
    this.removeLatest(json, state);
    this.removeOrder(json, state);
    this.removeUnread(json, state);
  }

  removeLatest(json, state) {
    let idx = _.findIndex(state.latest, json);
    _.pullAt(state.latest, [idx]);
  }

  removeUnread(json, state) {
    let idx = _.findIndex(state.latest, json);
    _.pullAt(state.latest, [idx]);
  }

  removeOrder(json, state) {
    if (json.who === window.ship) {
      if (state.pubs[json.coll]) {
        let pinIdx   = _.findIndex(state.pubs[json.coll].order.pin, json.post);
        let unpinIdx = _.findIndex(state.pubs[json.coll].order.unpin, json.post);
        
        if (pinIdx != -1) {
          _.pullAt(state.pubs[json.coll].order.pin, [pinIdx]);
        }
        if (unpinIdx != -1) {
          _.pullAt(state.pubs[json.coll].order.unpin, [unpinIdx]);
        }
      }
    } else {
      if (state.subs[json.who][json.coll]) {
        let pinIdx   = 
          _.findIndex(state.subs[json.who][json.coll].order.pin, json.post);
        let unpinIdx = 
          _.findIndex(state.subs[json.who][json.coll].order.unpin, json.post);
        
        if (pinIdx != -1) {
          _.pullAt(state.subs[json.who][json.coll].order.pin, [pinIdx]);
        }
        if (unpinIdx != -1) {
          _.pullAt(state.subs[json.who][json.coll].order.unpin, [unpinIdx]);
        }
      }
    }
  }

  reduceCollection(json, state) {
    if (json.who === window.ship) {
      state.pubs[json.coll] = {
        info: json.data,
        order: { pin: [], unpin: [] },
        posts: {},
      }
    } else {
      state.subs[json.who][json.coll] = {
        info: json.data,
        order: { pin: [], unpin: [] },
        posts: {},
      }
    }
  }

  reducePost(json, state) {
    let who = json.post.who;
    let coll = json.post.coll;
    let post = json.post.post;
    let data = json.post.data;

    if (who === window.ship) {
      if (state.pubs[coll].posts[post]) {
        state.pubs[coll].posts[post].post = data;
      } else {
        state.pubs[coll].posts[post] = {
          post: data,
          comments: [],
        };
      }
    } else {
      if (state.subs[who][coll].posts[post]) {
        state.subs[who][coll].posts[post].post = data;
      } else {
        state.subs[who][coll].posts[post] = {
          post: data,
          comments: [],
        };
      }
    }

    this.insertPost(json, state);
  }

  insertPost(json, state) {
    this.insertLatest(json, state); 
    this.insertUnread(json, state);
    this.insertOrder(json, state);
  }

  insertLatest(json, state) {
    let newIndex = {
      post: json.post.post,
      coll: json.post.coll,
      who: json.post.who,
    }
    let newDate = json.post.data.info["date-created"];

    if (state.latest.length == 0) {
      state.latest.push(newIndex);
      return;
    }

    if (state.latest.indexOf(newIndex) != -1) {
      return;
    }

    for (var i=0; i<state.latest.length; i++) {
      let postId = state.latest[i].post;
      let blogId = state.latest[i].coll;
      let ship = state.latest[i].who;

      if (newIndex.post == postId && newIndex.coll == blogId && newIndex.who == ship) {
        break;
      }

      let idate = this.retrievePost(state, blogId, postId, ship).info["date-created"];

      if (newDate >= idate) {
        state.latest.splice(i, 0, newIndex);
        break;
      } else if (i == (state.latest.length - 1)) {
        state.latest.push(newIndex);
        break;
      }
    }
  }

  insertUnread(json, state) {
    if (json.post.who != window.ship) {
      state.unread.push({
        post: json.post.post,
        coll: json.post.coll,
        who: json.post.who,
      });
    }
  }

  insertOrder(json, state) {
    let blogId = json.post.coll;
    let ship = json.post.who;
    let blog = this.retrieveColl(state, blogId, ship);
    let list = json.post.data.info.pinned
      ?  blog.order.pin
      :  blog.order.unpin;
    let newDate = json.post.data.info["date-created"];

    if (list.length == 0) {
      list.push(json.post.post);
    }

    if (list.indexOf(json.post.post) != -1) {
      return;
    }

    for (var i=0; i<list.length; i++) {
      let postId = list[i];
      if (json.post.post === postId) {
        break;
      }

      let idate = this.retrievePost(state, blogId, postId, ship).info["date-created"];

      if (newDate >= idate) {
        list.splice(i, 0, json.post.post);
        break;
      } else if (i == (state.latest.length - 1)) {
        list.push(json.post.post);
        break;
      }
    }

    if (window.ship == ship) {
      state.pubs[blogId].order = json.post.data.info.pinned
        ?  {pin: list, unpin: blog.order.unpin}
        :  {pin: blog.order.pin, unpin: list};
    } else {
      state.subs[ship][blogId].order = json.post.data.info.pinned
        ?  {pin: list, unpin: blog.order.unpin}
        :  {pin: blog.order.pin, unpin: list};
    }
  }

  retrieveColl(state, coll, who) {
    if (who === window.ship) {
      return state.pubs[coll];
    } else {
      return state.subs[who][coll];
    }
  }

  retrievePost(state, coll, post, who) {
    if (who === window.ship) {
      return state.pubs[coll].posts[post].post;
    } else {
      return state.subs[who][coll].posts[post].post;
    }
  }

  reduceComments(json, state) {
    let who = json.comments.who;
    let coll = json.comments.coll;
    let post = json.comments.post;
    let data = json.comments.data;

    if (who === window.ship) {
      if (state.pubs[coll].posts[post]) {
        state.pubs[coll].posts[post].comments = data;
      } else {
        state.pubs[coll].posts[post] = {
          post: null,
          comments: data,
        };
      }
    } else {
      if (state.subs[who][coll].posts[post]) {
        state.subs[who][coll].posts[post].comments = data;
      } else {
        state.subs[who][coll].posts[post] = {
          post: null,
          comments: data,
        };
      }
    }
  }

  reduceTotal(json, state) {
    if (json.total.who == window.ship) {
      state.pubs[json.total.coll] = json.total.data
    } else {
      if (state.subs[json.total.who]) {
        state.subs[json.total.who][json.total.coll] = json.total.data;
      } else {
        state.subs[json.total.who] = {
          [json.total.coll] : json.total.data
        }
      }
    }
    let posts = Object.keys(json.total.data.posts);
    for (var i=0; i<posts.length; i++) {
      let post = {
        post: {
          coll: json.total.coll,
          post: posts[i],
          who: json.total.who,
          data: json.total.data.posts[posts[i]].post,
        }
      };
      this.insertPost(post, state);
    }
  }
}
