import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path } from '../types/noun';
import { Contact, ContactEdit } from '../types/contact-update';

export default class ContactsApi extends BaseApi<StoreState> {

  create(path: Path, ships: Patp[] = [], title: string, description: string) {
    return this.viewAction({
      create: {
        path,
        ships,
        title,
        description
      }
    });
  }

  share(recipient: Patp, path: Patp, ship: Patp, contact: Contact) {
    return this.viewAction({
      share: {
        recipient, path, ship, contact
      }
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
        path, ship, 'edit-field': editField
      }
    });
  }

  private hookAction(data) {
    return this.action('contact-hook', 'contact-action', data);
  }

  private viewAction(data) {
    return this.action('contact-view', 'json', data);
  }
}
