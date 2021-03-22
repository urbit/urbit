import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp, Enc } from '@urbit/api';
import {
  GroupAction,
  GroupPolicy,
  Resource,
  Tag,
  GroupPolicyDiff
} from '@urbit/api/groups';
import { makeResource } from '../lib/group';

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

    return this.viewAction({ join: { resource, ship } });
  }

  create(name: string, policy: Enc<GroupPolicy>, title: string, description: string) {
    return this.viewThread('group-create', {
      create: {
        name,
        policy,
        title,
        description
      }
    });
  }

  deleteGroup(ship: string, name: string) {
    const resource = makeResource(ship, name);

    return this.viewThread('group-delete', {
      remove: resource
    });
  }

  leaveGroup(ship: string, name: string) {
    const resource = makeResource(ship, name);
    return this.viewThread('group-leave', {
      leave: resource
    });
  }

  invite(ship: string, name: string, ships: Patp[], description: string) {
    const resource = makeResource(ship, name);
    return this.viewThread('group-invite', {
      invite: {
        resource,
        ships,
        description
      }
    });
  }

  private proxyAction(action: GroupAction) {
    return this.action('group-push-hook', 'group-update', action);
  }

  private storeAction(action: GroupAction) {
    return this.action('group-store', 'group-update', action);
  }

  private viewThread(thread: string, action: any) {
    return this.spider('group-view-action', 'json', thread, action);
  }

  private viewAction(action: any) {
    return this.action('group-view', 'group-view-action', action);
  }
}
