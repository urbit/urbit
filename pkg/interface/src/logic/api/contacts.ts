import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp } from '@urbit/api';
import { ContactEdit } from '@urbit/api/contacts';

export default class ContactsApi extends BaseApi<StoreState> {
  add(ship: Patp, contact: any) {
    contact['last-updated'] = Date.now();
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
    {add-group: {ship, name}}
    {remove-group: {ship, name}}
    */
    return this.storeAction({
      edit: {
        ship,
        'edit-field': editField,
        timestamp: Date.now()
      }
    });
  }

  allowShips(ships: Patp[]) {
    return this.storeAction({
      allow: {
        ships
      }
    });
  }

  allowGroup(ship: string, name: string) {
    const group = { ship, name };
    return this.storeAction({
      allow: {
        group
      }
    });
  }

  setPublic(setPublic: any) {
    return this.storeAction({
      'set-public': setPublic
    });
  }

  share(recipient: Patp) {
    return this.action(
      'contact-push-hook',
      'contact-share',
      { share: recipient }
    );
  }

  fetchIsAllowed(entity, name, ship, personal) {
    const isPersonal = personal ? 'true' : 'false';
    return this.scry<any>(
      'contact-store',
      `/is-allowed/${entity}/${name}/${ship}/${isPersonal}`
    );
  }

  retrieve(ship: string) {
    const resource = { ship, name: '' };
    return this.action('contact-pull-hook', 'pull-hook-action', {
      add: {
        resource,
        ship
      }
    });
  }

  private storeAction(action: any): Promise<any> {
    return this.action('contact-store', 'contact-update', action);
  }

  private viewAction(threadName: string, action: any) {
    return this.spider('contact-view-action', 'json', threadName, action);
  }

  private hookAction(ship: Patp, action: any): Promise<any> {
    return this.action('contact-push-hook', 'contact-update', action);
  }
}
