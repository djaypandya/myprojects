import styles from './Contact.module.css';

export default function Contact() {
    return (
        <main className={styles.container}>
            <div className={styles.textWrapper}>
                <h1 className={styles.heading}>Let&apos;s talk about your space</h1>
                <p className={styles.text}>
                    We take on a limited number of projects each year to ensure every detail receives the attention it deserves.
                    Book a consultation to discuss your vision.
                </p>
            </div>

            <div className={styles.embedContainer}>
                {/* Placeholder for Calendly Embed */}
                <p>[ Calendly Booking Widget Loading... ]</p>
            </div>
        </main>
    );
}
