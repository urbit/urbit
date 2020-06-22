import BaseApi from './base';
import { StoreState } from '../store/type';
import { Path, Patp } from '../types/noun';


export default class GroupsApi extends BaseApi<StoreState> {
  add(path: Path, ships: Patp[] = []) {
    return this.action('group-store', 'group-action', {
      add: { members: ships, path }
    });
  }

  remove(path: Path, ships: Patp[] = []) {
    return this.action('group-store', 'group-action', {
      remove: { members: ships, path }
    });
  }
}

class PrivateHelper extends BaseApi {
  contactViewAction(data) {
    return this.action('contact-view', 'json', data);
  }

  contactCreate(path, ships = [], title, description) {
    return this.contactViewAction({
      create: {
        path,
        ships,
        title,
        description
      }
    });
  }





  contactShare(recipient, path, ship, contact) {
    return this.contactViewAction({
      share: {
        recipient, path, ship, contact
      }
    });
  }

  contactDelete(path) {
    return this.contactViewAction({ delete: { path } });
  }

  contactRemove(path, ship) {
    return this.contactViewAction({ remove: { path, ship } });
  }

  contactHookAction(data) {
    return this.action('contact-hook', 'contact-action', data);
  }

  contactEdit(path, ship, editField) {
    /* editField can be...
    {nickname: ''}
    {email: ''}
    {phone: ''}
    {website: ''}
    {notes: ''}
    {color: 'fff'}  // with no 0x prefix
    {avatar: null}
    {avatar: {url: ''}}
    */
    return this.contactHookAction({
      edit: {
        path, ship, 'edit-field': editField
      }
    });
  }

  inviteAction(data) {
    return this.action('invite-store', 'json', data);
  }

  inviteAccept(uid) {
    return this.inviteAction({
      accept: {
        path: '/contacts',
        uid
      }
    });
  }

  inviteDecline(uid) {
    return this.inviteAction({
      decline: {
        path: '/contacts',
        uid
      }
    });
  }

  metadataAction(data) {
    return this.action('metadata-hook', 'metadata-action', data);
  }

  metadataAdd(appPath, groupPath, title, description, dateCreated, color) {
    const creator = `~${window.ship}`;
    return this.metadataAction({
      add: {
        'group-path': groupPath,
        resource: {
          'app-path': appPath,
          'app-name': 'contacts'
        },
        metadata: {
          title,
          description,
          color,
          'date-created': dateCreated,
          creator
        }
      }
    });
  }

  setSelected(selected) {
    this.store.handleEvent({
      data: {
        local: {
          selected: selected
        }
      }
    });
  }
}

