import { stringToTa } from '../lib/util';

import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path } from '../types/noun';

export default class LinksApi extends BaseApi<StoreState> {


  getCommentsPage(path: Path, url: string, page: number) {
    const strictUrl = stringToTa(url);
    const endpoint = '/json/' + page + '/discussions/' + strictUrl + path;
    this.fetchLink(
      endpoint,
      (res) => {
        if (res.data['link-update']['initial-discussions']) {
          // these aren't returned with the response,
          // so this ensures the reducers know them.
          res.data['link-update']['initial-discussions'].path = path;
          res.data['link-update']['initial-discussions'].url = url;
        }
        this.store.handleEvent(res);
      },
      console.error,
      () => {} // no-op on quit
    );
  }

  getPage(path: Path, page: number) {
    const endpoint = '/json/' + page + '/submissions' + path;
    this.fetchLink(
      endpoint,
      (dat) => {
        this.store.handleEvent(dat);
      },
      console.error,
      () => {} // no-op on quit
    );
  }

  getSubmission(path: Path, url: string, callback) {
    const strictUrl = stringToTa(url);
    const endpoint = '/json/0/submission/' + strictUrl + path;
    this.fetchLink(
      endpoint,
      (res) => {
        if (res.data.submission) {
          callback(res.data.submission);
        } else {
          console.error('unexpected submission response', res);
        }
      },
      console.error,
      () => {} // no-op on quit
    );
  }



  createCollection(path, title, description, members, realGroup) {
    // members is either {group:'/group-path'} or {'ships':[~zod]},
    // with realGroup signifying if ships should become a managed group or not.
    return this.viewAction({
      create: { path, title, description, members, realGroup }
    });
  }

  deleteCollection(path) {
    return this.viewAction({
      delete: { path }
    });
  }

  inviteToCollection(path, ships) {
    return this.viewAction({
      invite: { path, ships }
    });
  }

  joinCollection(path) {
    return this.linkListenAction({ watch: path });
  }

  removeCollection(path) {
    return this.linkListenAction({ leave: path });
  }


  postLink(path: Path, url: string, title: string) {
    return this.linkAction({
      save: { path, url, title }
    });
  }

  postComment(path: Path, url: string, comment: string) {
    return this.linkAction({
      note: { path, url, udon: comment }
    });
  }

  // leave url as null to mark all under path as read
  seenLink(path: Path, url?: string) {
    return this.linkAction({
      seen: { path, url: url || null }
    });
  }

  private linkAction(data) {
    return this.action('link-store', 'link-action', data);
  }

  private viewAction(data) {
    return this.action('link-view', 'link-view-action', data);
  }

  private linkListenAction(data) {
    return this.action('link-listen-hook', 'link-listen-action', data);
  }

  private fetchLink(path: Path, result, fail, quit) {
    this.subscribe.bind(this)(
      path,
      'PUT',
      this.ship,
      'link-view',
      result,
      fail,
      quit
    );
  }
}
