import { cite } from '~/logic/lib/util';

  const indexes = new Map([
    ['commands', []],
    ['subscriptions', []],
    ['groups', []],
    ['apps', []],
    ['other', []]
  ]);

// result schematic
const result = function(title, link, app, host) {
  return {
    'title': title,
    'link': link,
    'app': app,
    'host': host
  };
};

const commandIndex = function (currentGroup) {
  // commands are special cased for default suite
  const commands = [];
  const workspace = currentGroup || '/home';
  commands.push(result(`Groups: Create`, `/~landscape/new`, 'Groups', null));
  commands.push(result(`Groups: Join`, `/~landscape/join`, 'Groups', null));
  commands.push(result(`Channel: Create`, `/~landscape${workspace}/new`, 'Groups', null));

  return commands;
};

const appIndex = function (apps) {
  // all apps are indexed from launch data
  // indexed into 'apps'
  const applications = [];
  Object.keys(apps)
    .filter((e) => {
      return apps[e]?.type?.basic;
    })
    .sort((a, b) => {
      return a.localeCompare(b);
    })
    .map((e) => {
      const obj = result(
        apps[e].type.basic.title,
        apps[e].type.basic.linkedUrl,
        apps[e].type.basic.title,
        null
      );
      applications.push(obj);
    });
  return applications;
};

const otherIndex = function() {
  const other = [];
  other.push(result('DMs + Drafts', '/~landscape/home', 'home', null));
  other.push(result('Notifications', '/~notifications', 'inbox', null));
  other.push(result('Profile and Settings', '/~profile/identity', 'profile', null));
  other.push(result('Log Out', '/~/logout', 'logout', null));

  return other;
};

export default function index(associations, apps, currentGroup, groups) {
  // all metadata from all apps is indexed
  // into subscriptions and landscape
  const subscriptions = [];
  const landscape = [];
  Object.keys(associations).filter((e) => {
    // skip apps with no metadata
    return Object.keys(associations[e]).length > 0;
    }).map((e) => {
      // iterate through each app's metadata object
      Object.keys(associations[e]).map((association) => {
        const each = associations[e][association];
        let title = each['app-path'];
        if (each.metadata.title !== '') {
          title = each.metadata.title;
        }

        let app = each['app-name'];
        if (each['app-name'] === 'contacts') {
          app = 'groups';
        };

        if (each['app-name'] === 'graph') {
          app = each.metadata.module;
        }

        const shipStart = each['app-path'].substr(each['app-path'].indexOf('~'));

        if (app === 'groups') {
          const obj = result(
            title,
            `/~landscape${each['app-path']}`,
            app.charAt(0).toUpperCase() + app.slice(1),
            cite(shipStart.slice(0, shipStart.indexOf('/')))
          );
          landscape.push(obj);
        } else {
          const app = each.metadata.module || each['app-name'];
          const group = (groups[each['group-path']]?.hidden)
            ? '/home' : each['group-path'];
          const obj = result(
            title,
            `/~landscape${group}/join/${app}${each['app-path']}`,
            app.charAt(0).toUpperCase() + app.slice(1),
            (associations?.contacts?.[each['group-path']]?.metadata?.title || null)
          );
          subscriptions.push(obj);
        }
      });
  });

  indexes.set('commands', commandIndex(currentGroup));
  indexes.set('subscriptions', subscriptions);
  indexes.set('groups', landscape);
  indexes.set('apps', appIndex(apps));
  indexes.set('other', otherIndex());

  return indexes;
};
