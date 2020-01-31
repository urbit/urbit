import React, { Component } from 'react';

//TODO Settings for owned notebooks
export class Settings extends Component {
  render() {
    return (
      <div>
        <div className="flex flex-column mb8">
          <label for="name" className="f9">Share</label>
          <small id="name-desc" className="f9 mb2 gray3">Share a link to this notebook</small>
          <div className="flex">
            <input style={{flex: "1"}} id="name" placeholder="dopzod.arvo.network/4f5hsS" className="input-reset bt bl bb pa3 gray4" type="text" aria-describedby="name-desc"/>
            <button className="bt br bb pa3 b--gray4">Copy</button>
          </div>
        </div>

        <div className="flex flex-column mb8">
          <label for="name" className="f9">Rename</label>
          <small id="name-desc" className="f9 mb2 gray3">Change the name of this notebook</small>
          <div className="flex">
            <input style={{flex: "1"}} id="name" placeholder="Notebook Name" className="input-reset ba pa3 gray4" type="text" aria-describedby="name-desc"/>
          </div>
        </div>
        <div className="flex flex-column">
          <label for="name" className="f9">Export</label>
          <small id="name-desc" className="f9 mb2 gray3">Change the name of this notebook</small>
          <button className="bg-black white pa3">
            <div className="flex justify-between">
              <div>Export Notebook</div>
              <div>↓</div>
            </div>
          </button>
        </div>


      </div>

    )
  }
}

export default Settings
