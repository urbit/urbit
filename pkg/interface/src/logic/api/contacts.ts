import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, Enc } from '~/types/noun';
import { Contact, ContactEdit } from '~/types/contact-update';
import { GroupPolicy, Resource } from '~/types/group-update';

export default class ContactsApi extends BaseApi<StoreState> {
  add(ship: Patp, contact: any) {
    return this.storeAction({ add: { ship, contact } });
  }

  remove(ship: Patp) {
    return this.storeAction({ remove: { ship } });
  }

  edit(ship: Patp, editField: ContactEdit) {
    /* editField can be...
    {nickname: ''}
    {email: ''}
    {phone: ''}
    {website: ''}
    {color: 'fff'}  // with no 0x prefix
    {avatar: null}
    {avatar: ''}
    */
    console.log(ship, editField);
    return this.storeAction({
      edit: {
        ship,
        'edit-field': editField,
      },
    });
  }

  setPublic(setPublic: any) {
    return this.storeAction({
      'set-public': setPublic
    });
  }

  private storeAction(action: any): Promise<any> {
    return this.action('contact-store', 'contact-update', action)
  }

  private viewAction(threadName: string, action: any) {
    return this.spider('contact-view-action', 'json', threadName, action);
  }

  private hookAction(ship: Patp, action: any): Promise<any> {
    return this.action('contact-push-hook', 'contact-update', action);
  }
}
