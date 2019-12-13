import React, { Component } from 'react';
import { Sigil } from './icons/sigil';
import { uxToHex } from '/lib/util';

import { api } from '/api';
import { Route, Link } from 'react-router-dom';

export class ContactCard extends Component {
  constructor() {
    super();
    this.state = {
      edit: false,
      colorToSet: "",
      nickNameToSet: ""
    }
    this.editToggle = this.editToggle.bind(this);
    this.sigilColorSet = this.sigilColorSet.bind(this);
    this.nickNameToSet = this.nickNameToSet.bind(this);
    this.setField = this.setField.bind(this);

  }

  componentDidUpdate() {
    let currentColor = (this.props.contact.color) ? this.props.contact.color : "0x0";
    let currentHex = uxToHex(currentColor);
    let hexExp = /#?([0-9A-Fa-f]{6})/
    let hexTest = hexExp.exec(this.state.colorToSet);

    if ((hexTest) && (hexTest[1] !== currentHex)) {
      let ship = "~" + this.props.ship;
      api.contactEdit(this.props.path, ship, {color: hexTest[1]});
    }
  }

  editToggle() {
    let editSwitch = this.state.edit;
    editSwitch = !editSwitch;
    this.setState({edit: editSwitch});
  }

  nickNameToSet(event) {
    this.setState({ nickNameToSet: event.target.value });
  }

  shipParser(ship) {
    switch (ship.length) {
      case 3: return "Galaxy";
      case 6: return "Star";
      case 13: return "Planet";
      case 56: return "Comet";
      default: return "Unknown";
    }
  }

  setField(field) {
    let ship = "~" + this.props.ship;
    switch (field) {
      case "nickname": {
        if ((this.state.nickNameToSet === "")
        || (this.state.nickNameToSet === this.props.contact.nickname)) {
          return false;
        }
        api.contactEdit(this.props.path, ship, { nickname: this.state.nickNameToSet });
        break;
      }
      case "removeAvatar": {
        api.contactEdit(this.props.path, ship, { avatar: null });
        break;
      }
    }
  }

  sigilColorSet(event) {
    this.setState({ colorToSet: event.target.value });
  }

  renderEditCard() {

    let defaultValue = {
      nickname: (this.props.share) 
      ? this.props.rootIdentity.nickname
      : this.props.contact.nickname,
      email: (this.props.share)
      ? this.props.rootIdentity.email
      : this.props.contact.email,
      phone: (this.props.share)
      ? this.props.rootIdentity.phone
      : this.props.contact.phone,
      website: (this.props.share)
      ? this.props.rootIdentity.website
      : this.props.contact.phone,
      notes: (this.props.share)
      ? this.props.rootIdentity.notes
      : this.props.contact.notes
    }

    let shipType = this.shipParser(this.props.ship);

    let currentColor = (this.props.contact.color) ? this.props.contact.color : "0x0";
    let hexColor = uxToHex(currentColor);

    let sigilColor = ""

    if (!hasAvatar) { 
      sigilColor = (
        <div className="tl mt4 mb4 w-auto ml-auto mr-auto"
        style={{ width: "fit-content" }}>
        <p className="f9 gray2 lh-copy">Sigil Color</p>
          <textarea
           className="b--gray4 black f7 ba db pl2"
           onChange={this.sigilColorSet}
           defaultValue={"#" + hexColor}
           style={{
             resize: "none",
             height: 40,
             paddingTop: 10,
              width: 114
            }}></textarea>
        </div>
      )
      //TODO The fields to actually edit, using the api hooks for those atomic actions
    }

    let removeImage = "";
    let avatar = (hasAvatar) 
      ? <img className="dib h-auto" width={128} src={this.props.contact.avatar} /> 
      : <Sigil ship={this.props.ship} size={128} color={"#" + hexColor} />;

    if (hasAvatar) {
      removeImage = (
        <div>
          <button class="f9 black pointer db"
            onClick={() => this.setField("removeAvatar")}>
            Remove photo
            </button>
        </div>
      )
    }

    return (
      <div className="w-100 flex justify-center pa4 pa0-xl pt4-xl">
        <div className="w-100 mw6 tc">
          {avatar}
          {sigilColor}
          <button className="f9 b--black ba pa2">Upload an Image</button>
          {removeImage}

          <div className="w-100 pt8 lh-copy tl">
            <p className="f9 gray2">Ship Name</p>
            <p className="f8 mono">~{this.props.ship}</p>
            <p className="f9 gray2 mt3">Ship Type</p>
            <p className="f8">{shipType}</p>

            <hr className="mv8 gray4 b--gray4 bb-0 b--solid" />
            <p className="f9 gray2">Nickname</p>
              <div className="w-100 flex">
                <textarea
                 className="w-100 ba pl3 b--gray4"
                 style={{ resize: "none",
                          height: 40,
                          paddingTop: 10 }}
                 onChange={this.nickNameToSet}
                defaultValue={defaultValue.nickname}
                 />
                <button className={"f9 ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                ((this.props.contact.nickname === "") ? "dn" : "dib")}>
                Delete
                </button>
              </div>
              <button className={"db mv2 f9 ba pa2 pl3 pr3 " +
              (((this.props.contact.nickname === this.state.nickNameToSet)
              || (this.state.nickNameToSet === ""))
              ? "b--gray4 gray4" 
              : "b--black")}
              onClick={() => this.setField("nickname")}>
                Save
              </button>
            <p className="f9 gray2">Email</p>
            <p className="f9 gray2">Phone</p>
            <p className="f9 gray2">Website</p>
            <p className="f9 gray2">Notes</p>

          </div>
        </div>
      </div>
    )
  }

  renderCard() {
    let shipType = this.shipParser(this.props.ship);
    let currentColor = (this.props.contact.color) ? this.props.contact.color : "0x0";
    let hexColor = uxToHex(currentColor);

    const hasAvatar = (this.props.contact.avatar !== "TODO");

    let avatar = (hasAvatar)
      ? <img className="dib h-auto" width={128} src={this.props.contact.avatar} />
      : <Sigil ship={this.props.ship} size={128} color={"#" + hexColor} />;

    return (
      <div className="w-100 flex justify-center pa4 pa0-xl pt4-xl">
        <div className="w-100 mw6 tc">
          {avatar}
          <div className="w-100 pt8 lh-copy tl">
            <p className="f9 gray2">Ship Name</p>
            <p className="f8 mono">~{this.props.ship}</p>
            <p className="f9 gray2 mt3">Ship Type</p>
            <p className="f8">{shipType}</p>

            <hr className="mv8 gray4 b--gray4 bb-0 b--solid" />
            <div>
              {(() => {
                if (this.props.contact.nickname) {
                  return (
                    <div>
                      <p className="f9 gray2">Nickname</p>
                      <p className="f8">{this.props.contact.nickname}</p>
                    </div>
                  )
                }

                if (this.props.contact.email) {
                  return (
                    <div>
                      <p className="f9 gray2">Email</p>
                      <p className="f8">{this.props.contact.email}</p>
                    </div>
                  )
                }
                if (this.props.contact.phone) {
                  return (
                    <div>
                      <p className="f9 gray2">Phone</p>
                      <p className="f8">{this.props.contact.phone}</p>
                    </div>
                  )
                }
                if (this.props.contact.website) {
                  return (
                    <div>
                      <p className="f9 gray2">Website</p>
                      <a className="bb b--black f8" href={this.props.contact.website}>{this.props.contact.website}</a>
                    </div>
                  )
                }
                if (this.props.contact.notes) {
                  return (
                    <div>
                      <p className="f9 gray2">Notes</p>
                      <p className="f8">{this.props.contact.notes}</p>
                    </div>
                  )
                }
              })()}
            </div>
          </div>
        </div>
      </div>
    )
  }

    render() {

      let ourOption = (this.props.ship === window.ship)
      ? "dib"
      : "dn";

      let localOption = ((this.props.ship === window.ship) && (this.props.path === "/~/default"))
      ? "dib"
      : "dn";

      let editInfoText = (this.state.edit)
      ? "Finish Editing"
      : "Edit Contact Info";

      let adminOption = (this.props.path.includes(window.ship) && (this.props.ship !== window.ship))
      ? "dib"
      : "dn";

      let card = (this.state.edit)
      ? this.renderEditCard()
      : this.renderCard();

      //TODO "Share card" if it's /me -> sends to /~/default of recipient
        return (
            <div className="h-100 w-100 overflow-x-hidden">
            <div className="w-100 h2 dn-m dn-l dn-xl inter pb6 pl3 pt3 f8">
              <Link to="/~contacts/">{"⟵"}</Link>
            </div>
            <div className="w-100 bb b--gray4">
              <button 
                onClick={this.editToggle}
              className={`ml3 mt2 mb2 f9 pa2 ba br3 pointer b--black ` + ourOption}>
                {editInfoText}
              </button>
              <button className={`ml3 mt2 mb2 f9 pa2 ba br3 b--black ` + localOption}>
                Share Contact Info
              </button>
              <button className={`ml3 mt2 mb2 f9 pa2 ba red2 br3 b--red2 ` + adminOption}
              onClick={this.removeContact}>
                Remove from Group
              </button>
            </div>
              {card}
            </div>
        )
    }
}

export default ContactCard
