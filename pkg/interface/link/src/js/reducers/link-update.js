import _ from 'lodash';

// page size as expected from link-view.
// must change in parallel with the +page-size in /app/link-view to
// ensure sane behavior.
const PAGE_SIZE = 25;

export class LinkUpdateReducer {
  reduce(json, state) {
    this.submissionsPage(json, state);
    this.submissionsUpdate(json, state);
    this.discussionsPage(json, state);
    this.discussionsUpdate(json, state);
    this.observationUpdate(json, state);
  }

  submissionsPage(json, state) {
    let data = _.get(json, 'initial-submissions', false);
    if (data) {
      //  { "initial-submissions": {
      //    "/~ship/group": {
      //      page: [{ship, timestamp, title, url}]
      //      page-number: 0
      //      total-items: 1
      //      total-pages: 1
      //    }
      //  } }

      for (var path of Object.keys(data)) {
        const here = data[path];
        const page = here.pageNumber;

        // if we didn't have any state for this path yet, initialize.
        if (!state.links[path]) {
          state.links[path] = {local: {}};
        }

        // since data contains an up-to-date full version of the page,
        // we can safely overwrite the one in state.
        if (typeof page === 'number' && here.page) {
          state.links[path][page] = here.page;
          state.links[path].local[page] = false;
        }
        state.links[path].totalPages = here.totalPages;
        state.links[path].totalItems = here.totalItems;
        state.links[path].unseenCount = here.unseenCount;

        // write seen status to a separate structure,
        // for easier modification later.
        if (!state.seen[path]) {
          state.seen[path] = {};
        }
        (here.page || []).map(submission => {
          state.seen[path][submission.url] = submission.seen;
        });
      }
    }
  }

  submissionsUpdate(json, state) {
    let data = _.get(json, 'submissions', false);
    if (data) {
      //  { "submissions": {
      //    path: /~ship/group
      //    pages: [{ship, timestamp, title, url}]
      //  } }

      const path = data.path;

      // stub in a comment count, which is more or less guaranteed to be 0
      data.pages = data.pages.map(submission => {
        submission.commentCount = 0;
        state.seen[path][submission.url] = false;
        return submission;
      });


      // add the new submissions to state, update totals
      state.links[path] = this._addNewItems(
        data.pages, state.links[path]
      );
      state.links[path].unseenCount =
        (state.links[path].unseenCount || 0) + data.pages.length;
    }
  }

  discussionsPage(json, state) {
    let data = _.get(json, 'initial-discussions', false);
    if (data) {
      //  { "initial-discussions": {
      //    path: "/~ship/group"
      //    url: https://urbit.org/
      //    page: [{ship, timestamp, title, url}]
      //    page-number: 0
      //    total-items: 1
      //    total-pages: 1
      //  } }

      const path = data.path;
      const url = data.url;
      const page = data.pageNumber;

      // if we didn't have any state for this path yet, initialize.
      if (!state.comments[path]) {
        state.comments[path] = {};
      }
      if (!state.comments[path][url]) {
        state.comments[path][url] = {local: {}};
      }
      let here = state.comments[path][url];

      // since data contains an up-to-date full version of the page,
      // we can safely overwrite the one in state.
      here[page] = data.page;
      here.local[page] = false;
      here.totalPages = data.totalPages;
      here.totalItems = data.totalItems;
    }
  }

  discussionsUpdate(json, state) {
    let data = _.get(json, 'discussions', false);
    if (data) {
      //  { "discussions": {
      //    path: /~ship/path
      //    url: 'https://urbit.org'
      //    comments: [{ship, timestamp, udon}]
      //  } }

      const path = data.path;
      const url = data.url;

      // add new comments to state, update totals
      state.comments[path][url] = this._addNewItems(
        data.comments, state.comments[path][url]
      );
    }
  }

  observationUpdate(json, state) {
    const data = _.get(json, 'observation', false);
    if (data) {
      //  { "observation": {
      //    path: /~ship/path
      //    urls: ['https://urbit.org']
      //  } }

      const path = data.path;
      if (!state.seen[path]) {
        state.seen[path] = {};
      }
      let seen = state.seen[path];

      // mark urls as seen
      data.urls.map(url => {
        seen[url] = true;
      });
      if (state.links[path]) {
        state.links[path].unseenCount =
          state.links[path].unseenCount - data.urls.length;
      }
    }
  }

//

  _addNewItems(items, pages, page = 0) {
    if (!pages) {
      pages = {
        local: {},
        totalPages: 0,
        totalItems: 0
      };
    }
    const i = page;
    if (!pages[i]) {
      pages[i] = [];
      // if we know this page exists in the backend, flag it as "local",
      // so that we know to initiate a "fetch the rest" request when we want
      // to display the page.
      pages.local[i] = (page < pages.totalPages);
    }
    pages[i] = items.concat(pages[i]);
    pages[i].sort((a, b) => b.time - a.time);
    pages.totalItems = pages.totalItems + items.length;
    if (pages[i].length <= PAGE_SIZE) {
      pages.totalPages = Math.ceil(pages.totalItems / PAGE_SIZE);
      return pages;
    }
    // overflow into next page
    const tail = pages[i].slice(PAGE_SIZE);
    pages[i].length = PAGE_SIZE;
    pages.totalItems = pages.totalItems - tail.length;
    return this._addNewItems(tail, pages, page+1);
  }

}