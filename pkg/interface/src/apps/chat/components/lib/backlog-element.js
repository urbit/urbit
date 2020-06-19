import React, { Component } from 'react';

export class BacklogElement extends Component {
  render() {
    return (
      <div className="center mw6">
        <div className="db pa3 ma3 ba b--gray4 bg-gray5 b--gray2-d bg-gray1-d white-d flex items-center">
            <img className="invert-d spin-active v-mid"
              src="/~chat/img/Spinner.png"
              width={16}
              height={16}
            />
            <p className="lh-copy db ml3">
            Past messages are being restored
            </p>
        </div>
      </div>

    );
  }
}
