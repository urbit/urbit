import React from 'react';
import { useLocation, Link } from 'react-router-dom';

import GroupFilter from './GroupFilter';
import { Sigil } from '../lib/sigil';

const getLocationName = (basePath) => {
  if (basePath === '~chat')
    return 'Chat';
  else if (basePath === '~dojo')
    return 'Dojo';
  else if (basePath === '~groups')
    return 'Groups';
  else if (basePath === '~link')
    return 'Links';
  else if (basePath === '~publish')
    return 'Publish';
  else
    return 'Unknown';
};

const StatusBar = (props) => {
  const location = useLocation();
  const basePath = location.pathname.split('/')[1];
  const locationName = location.pathname === '/'
    ? 'Home'
    : getLocationName(basePath);

  const display = (!window.location.href.includes('popout/') &&
    (locationName !== 'Unknown'))
      ? 'db' : 'dn';

  const invites = (props.invites && props.invites['/contacts'])
    ? props.invites['/contacts']
    : {};
  const connection = props.connection || 'connected';

  const reconnect = props.subscription.restart.bind(props.subscription);

  return (
    <div
      className={
        'bg-white bg-gray0-d w-100 justify-between relative tc pt3 ' + display
      }
      style={{ height: 45 }}
    >
      <div className="fl lh-copy absolute left-0 pl4" style={{ top: 8 }}>
      <Link to="/~groups/me"
          className="dib v-top" style={{ lineHeight: 0, paddingTop: 6 }}>
          <Sigil
            ship={'~' + window.ship}
            classes="v-mid mix-blend-diff"
            size={16}
            color={'#000000'}
          />
      </Link>
      <GroupFilter invites={invites} associations={props.associations} api={props.api} />
      <span className="dib f9 v-mid gray2 ml1 mr1 c-default inter">/</span>
        {
          location.pathname === '/'
            ? null
            : <Link
                className="dib f9 v-mid inter ml2 no-underline white-d"
                to="/"
                style={{ top: 14 }}
              >
              ⟵
              </Link>
        }
         <p className="dib f9 v-mid inter ml2 white-d">{locationName}</p>
    { connection === 'disconnected' && 
      (<span 
        onClick={reconnect}
        className="ml4 ph2 dib f9 v-mid red2 inter ba b-red2 br1 pointer"
        >Reconnect ↻</span> )
    }
    { connection === 'reconnecting' &&
      (<span className="ml4 ph2 dib f9 v-mid yellow2 inter ba b-yellow2 br1">Reconnecting</span> )
    }
      </div>
    </div>
  );
};

export default StatusBar;
