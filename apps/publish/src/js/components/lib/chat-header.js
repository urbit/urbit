import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import Mousetrap from 'mousetrap';
import classnames from 'classnames';

import { Sigil } from '/components/lib/icons/sigil';
import { isUrl, uuid, isDMStation } from '/lib/util';


export class ChatHeader extends Component {
  render() {
    return (
      <div className="w-100 pa2 mb3 cf">
        <div className="fl">
          <p className="f3 sans-serif">{this.props.title}</p>
          <div>
            <p className="dib mid-gray mr2 sans-serif">{this.props.numPeople} Participants</p>
          </div>
        </div>
        <div className="fr">
        </div>
      </div>
    );
  }
}
