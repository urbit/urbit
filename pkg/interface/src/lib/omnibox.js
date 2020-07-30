import defaultApps from './default-apps';

export default function index(associations, apps) {
  const index = new Map([
    ['commands', []],
    ['subscriptions', []],
    ['groups', []],
    ['apps', []]
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

  // commands are special cased for default suite
  const commands = [];
  defaultApps.filter((e) => {
    return (e !== 'dojo');
  }).map((e) => {
    let title = e;
    if (e === 'link') {
      title = 'Links';
    }

    title = title.charAt(0).toUpperCase() + title.slice(1);

    let obj = result(`${title}: Create`, `/~${e}/new`, title, null);
    commands.push(obj);

    if (title === 'Groups') {
      obj = result(`${title}: Join Group`, `/~${e}/join`, title, null);
      commands.push(obj);
    }
  });
  index.set('commands', commands);

  // all metadata from all apps is indexed
  // into subscriptions and groups
  const subscriptions = [];
  const groups = [];
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

        const shipStart = each['app-path'].substr(each['app-path'].indexOf('~'));

        if (app === 'groups') {
          const obj = result(
            title,
            `/~${app}${each['app-path']}`,
            app.charAt(0).toUpperCase() + app.slice(1),
            shipStart.slice(0, shipStart.indexOf('/'))
          );
          groups.push(obj);
        } else {
          let endpoint = '';
          if (app === 'chat') {
            endpoint = '/room';
          } else if (app === 'publish') {
            endpoint = '/notebook';
          }
          const obj = result(
            title,
            `/~${each['app-name']}${endpoint}${each['app-path']}`,
            app.charAt(0).toUpperCase() + app.slice(1),
            shipStart.slice(0, shipStart.indexOf('/'))
          );
          subscriptions.push(obj);
        }
      });
  });
  index.set('subscriptions', subscriptions);
  index.set('groups', groups);

  // all apps are indexed from launch data
  // indexed into 'apps'
  const applications = [];
  Object.keys(apps).filter((e) => {
    return (apps[e]?.type?.basic);
    }).map((e) => {
      const obj = result(apps[e].type.basic.title, apps[e].type.basic.linkedUrl, apps[e].type.basic.title, null);
      applications.push(obj);
  });
  index.set('apps', applications);

  return index;
};
