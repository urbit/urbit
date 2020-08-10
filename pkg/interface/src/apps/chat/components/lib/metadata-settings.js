
import MetadataInput from './metadata-input';

const MetadataSettings = (props) => {
  const {
    isOwner,
    association:
      (props.association) && ('metadata' in props.association) ?
      props.association : {},
    changeTitle
  } = props;




  return (
    <div>
      <MetadataInput
        title={'Rename'}
        description={'Change the name of this chat'}
        isDisabled={!isOwner}
              isDisabled,
      changeValue,
      title,
      description

      />      
      <div className={'w-100 pb6 fl mt3 ' + ((isOwner) ? '' : 'o-30')}>
      <p className="f8 mt3 lh-copy">Rename</p>
      <p className="f9 gray2 db mb4">Change the name of this chat</p>
      <div className="relative w-100 flex"
      style={{ maxWidth: '29rem' }}
      >
        <input
          className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
          'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
          value={state.title}
          disabled={!isOwner}
          onBlur={() => {
            if (isOwner) {
              // get title
              changeTitle();
            }
          }}
        />
        </div>
        <p className="f8 mt3 lh-copy">Change description</p>
        <p className="f9 gray2 db mb4">Change the description of this chat</p>
        <div className="relative w-100 flex"
          style={{ maxWidth: '29rem' }}
        >
          <input
            className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
              'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
            value={state.description}
            disabled={!isOwner}
            onChange={this.changeDescription}
            onBlur={() => {
              if (isOwner) {
                this.setState({ awaiting: true, type: 'Editing chat...' }, (() => {
                  props.api.metadata.metadataAdd(
                    'chat',
                    association['app-path'],
                    association['group-path'],
                    association.metadata.title,
                    state.description,
                    association.metadata['date-created'],
                    uxToHex(association.metadata.color)
                  ).then(() => {
                    this.setState({ awaiting: false });
                  });
                }));
              }
            }}
          />
        </div>
        <p className="f8 mt3 lh-copy">Change color</p>
        <p className="f9 gray2 db mb4">Give this chat a color when viewing group channels</p>
        <div className="relative w-100 flex"
          style={{ maxWidth: '10rem' }}
        >
          <div className="absolute"
            style={{
              height: 16,
              width: 16,
              backgroundColor: state.color,
              top: 13,
              left: 11
              }}
          />
          <input
            className={'pl7 f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
              'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
            value={state.color}
            disabled={!isOwner}
            onChange={this.changeColor}
            onBlur={this.submitColor}
          />
        </div>
      </div>
    </div>
  );
};


