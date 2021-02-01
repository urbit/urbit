import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp, Enc } from '~/types/noun';
import {
  GroupAction,
  GroupPolicy,
  Resource,
  Tag,
  GroupPolicyDiff,
} from '~/types/group-update';
import {makeResource} from '../lib/group';

export default class GroupsApi extends BaseApi<StoreState> {
  remove(resource: Resource, ships: Patp[]) {
    return this.proxyAction({ removeMembers: { resource, ships } });
  }

  addTag(resource: Resource, tag: Tag, ships: Patp[]) {
    return this.proxyAction({ addTag: { resource, tag, ships } });
  }

  removeTag(resource: Resource, tag: Tag, ships: Patp[]) {
    return this.proxyAction({ removeTag: { resource, tag, ships } });
  }

  add(resource: Resource, ships: Patp[]) {
    return this.proxyAction({ addMembers: { resource, ships } });
  }

  removeGroup(resource: Resource) {
    return this.storeAction({ removeGroup: { resource } });
  }

  changePolicy(resource: Resource, diff: Enc<GroupPolicyDiff>) {
    return this.proxyAction({ changePolicy: { resource, diff } });
  }

  join(ship: string, name: string) {
    const resource = makeResource(ship, name);

    return this.viewAction({ join: { resource, ship }});
  }

  private proxyAction(action: GroupAction) {
    return this.action('group-push-hook', 'group-update', action);
  }

  private storeAction(action: GroupAction) {
    return this.action('group-store', 'group-update', action);
  }

  private viewAction(action: any) {
    return this.action('group-view', 'group-view-action', action);

  }
}
