import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, Enc } from '../types/noun';
import { Contact, ContactEdit } from '../types/contact-update';
import { GroupPolicy, Resource } from '../types/group-update';

export default class ContactsApi extends BaseApi<StoreState> {
  create(
    name: string,
    policy: Enc<GroupPolicy>,
    title: string,
    description: string
  ) {
    return this.viewAction({
      create: {
        name,
        policy,
        title,
        description,
      },
    });
  }

  share(recipient: Patp, path: Patp, ship: Patp, contact: Contact) {
    return this.viewAction({
      share: {
        recipient,
        path,
        ship,
        contact,
      },
    });
  }

  delete(path: Path) {
    return this.viewAction({ delete: { path } });
  }

  remove(path: Path, ship: Patp) {
    return this.viewAction({ remove: { path, ship } });
  }

  edit(path: Path, ship: Patp, editField: ContactEdit) {
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
    return this.hookAction({
      edit: {
        path,
        ship,
        'edit-field': editField,
      },
    });
  }

  invite(resource: Resource, ship: Patp, text = '') {
    return this.viewAction({
      invite: { resource, ship, text },
    });
  }

  join(resource: Resource) {
    return this.viewAction({
      join: resource,
    });
  }

  private hookAction(data) {
    return this.action('contact-hook', 'contact-action', data);
  }

  private viewAction(data) {
    return this.action('contact-view', 'json', data);
  }
}
