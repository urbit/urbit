import BaseApi from './base';


export default class GroupsApi {
  constructor(ship, channel, store) {
    const helper = new PrivateHelper(ship, channel, store);

    this.ship = ship;
    this.subscribe = helper.subscribe.bind(helper);

    this.contactHook = {
      edit: helper.contactEdit.bind(helper)
    };

    this.contactView = {
      create: helper.contactCreate.bind(helper),
      delete: helper.contactDelete.bind(helper),
      remove: helper.contactRemove.bind(helper),
      share: helper.contactShare.bind(helper)
    };

    this.group = {
      add: helper.groupAdd.bind(helper),
      delete: helper.groupRemove.bind(helper)
    };

    this.invite = {
      accept: helper.inviteAccept.bind(helper),
      decline: helper.inviteDecline.bind(helper)
    };
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

  groupAdd(path, ships = []) {
    return this.action('group-store', 'group-action', {
      add: { members: ships, path }
    });
  }

  groupRemove(path, ships) {
    return this.action('group-store', 'group-action', {
      remove: { members: ships, path }
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

