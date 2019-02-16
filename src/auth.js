import config from './config';

export function startAuth (app) {
    const handler = Plaid.create({
        clientName: 'Budgeting',
        env: 'sandbox',
        key: config.publicToken,
        product: [ 'transactions' ],
        apiVersion: 'v2',
        // Optional – use webhooks to get transaction and error updates
        // webhook: 'https://requestb.in',
        onLoad: function () {
            // Optional, called when Link loads
        },
        onSuccess: function (publicToken, metadata) {
            console.log('Link Metadata', metadata);
            app.ports.linkResponse.send({
                data: {
                    publicToken,
                    linkSessionId: metadata.link_session_id,
                    accounts: metadata.accounts,
                    institution: metadata.institution
                }
            });
        },
        onExit: function (err, metadata) {
            if (err !== null) {
                // The user encountered a Plaid API error prior to exiting.
                return;
            }
            app.ports.linkResponse.send({
                errors: [ {
                    code: 'CANCELED_LINK'
                } ]
            });
        },
        onEvent: function (eventName, metadata) { }
    });

    handler.open();
};
