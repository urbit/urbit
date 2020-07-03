import BaseApi from "./base";
import { StoreState } from "../store/type";
import { SelectedGroup } from "../types/local-update";



export default class LocalApi extends BaseApi<StoreState> {
  setSelected(selected: SelectedGroup[]) {
    this.store.handleEvent({
      data: {
        local: {
          selected
        }
      }
    })
  }

  sidebarToggle() {
    this.store.handleEvent({
      data: {
        local: {
          sidebarToggle: true
        }
      }
    })
  }

}
